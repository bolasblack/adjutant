(ns utils.airtable
  (:refer-clojure :exclude [filter])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require ["airtable" :as Airtable]
            [cljs.core.async :as async :refer [>! <! put! take! close!]]
            [utils.core :as uc]
            [utils.async :as ua]
            [utils.string :as us]
            [clojure.string :as str]))

(def RATE_LIMIT 5)



(defn- create-rate-limit-chan []
  (let [chan (async/chan RATE_LIMIT)]
    (go-loop []
      (dotimes [n 5]
        (>! chan :start))
      (<! (async/timeout (* 1000 60)))
      (recur))
    chan))

(defn- serialize-fields [base fields]
  (clj->js
   (us/walk-keys
    (fn [key]
      (cond
        (some #(= key %) (:start-case-fields base))
        (us/start-case (name key))

        (some #(= key %) (:first-upper-fields base))
        (us/sentence-case (name key))

        (some #(= key %) (:upper-fields base))
        (str/upper-case (name key))

        :else
        (if-let [serializer (:field-name-serializer base)]
          (serializer key)
          key)))
    fields)))

(defn- deserialize-fields [base fields]
  (us/kebab-case-keys (js->clj fields :keywordize-keys true) :keep-pred? true))

(defn- deserialize-record [base record]
  (-> (.-fields record)
      (#(deserialize-fields base %))
      (assoc :_id (.-id record))))



(defn format-formula [& opt-coll]
  (letfn [(expr? [a]
            (and (coll? a)
                 (keyword? (first a))))
          (gen-find-subformula [key val]
            (str "FIND(\"" val "\", " (name key) ")"))
          (gen-equal-subformula [key val]
            (str "{" (name key) "}=" val))
          (gen-subexpr [key fn-name vals]
            (condp = fn-name
              :or (as-> vals $
                    (map #(gen-subformula key %) $)
                    (str "OR(" (str/join ", " $) ")"))
              :and (as-> vals $
                     (map #(gen-subformula key %) $)
                     (str "AND(" (str/join ", " $) ")"))
              (uc/error! (str "Unsupported fn-name: " fn-name))))
          (gen-subformula [key val]
            (cond
              (expr? val) (gen-subexpr key (first val) (next val))
              (string? val) (gen-find-subformula key val)
              :else (gen-equal-subformula key val)))]
    (let [conditions (cond
                       (and (= 1 (count opt-coll))
                            (map? (first opt-coll)))
                       (->> (first opt-coll)
                            (into [])
                            (apply concat))

                       (or (= 0 (count opt-coll))
                           (and (coll? (first opt-coll))
                                (empty? (first opt-coll))))
                       []

                       :else
                       opt-coll)
          opt-pairs (partition 2 conditions)
          subformulas (map #(gen-subformula (first %) (last %)) opt-pairs)]
      (if (empty? subformulas)
        ""
        (str "AND(" (str/join ", " subformulas) ")")))))



(defn base [& {:keys [api-key endpoint-url base-id
                      start-case-fields first-upper-fields
                      upper-fields field-name-serializer]
               :or {api-key js/process.env.AIRTABLE_API_KEY
                    start-case-fields []
                    first-upper-fields []
                    upper-fields []}
               :as opts}]
  {:pre [(some? base-id)]}
  (.configure Airtable #js {:apiKey api-key
                            :endpointUrl endpoint-url})
  {:base (.base Airtable base-id)
   :start-case-fields start-case-fields
   :first-upper-fields first-upper-fields
   :upper-fields upper-fields
   :field-name-serializer field-name-serializer})



(defn filter [base table-name & {:keys [limit formula]
                                 :or {limit js/Infinity}
                                 :as opts}]
  (let [chan (async/chan)
        sent-count-ref (atom 0)
        conditon (-> (dissoc opts :limit :formula)
                     (assoc :filterByFormula (format-formula formula))
                     clj->js)]
    (-> ((:base base) table-name)
        (.select conditon)
        (.eachPage
         (fn [records fetch-next-page]
           (.forEach
            records
            (fn [record]
              (let [sent-count (deref sent-count-ref)]
                (when (< sent-count limit)
                  (put! chan (deserialize-record base record))
                  (swap! sent-count-ref #(+ % 1))))))
           (if (< (deref sent-count-ref) limit)
             (fetch-next-page)
             (close! chan)))
         (fn [err]
           (when err (put! chan err))
           (close! chan))))
    chan))

(defn all [base table-name]
  (filter base table-name :formula "1 = 1"))



(defn fetch [base table-name id]
  (ua/go-try-let [record (ua/<p? (.find
                                  ((:base base) table-name)
                                  id))]
    (deserialize-record base record)))



(defn insert! [base table-name & records]
  (ua/limit-map
   (fn [record]
     (ua/go-try-let [record (ua/<p? (.create
                                     ((:base base) table-name)
                                     (serialize-fields base record)))]
       (deserialize-record base record)))
   records
   (create-rate-limit-chan)))



(defmulti update! (fn [base table-name ids update]
                    (if (coll? ids) :coll :single)))

(defmethod update! :single [base table-name id update]
  (ua/go-try-let [record (ua/<? (fetch base table-name id))
                  updated-record (ua/<p? (.update
                                          ((:base base) table-name)
                                          (:_id record)
                                          (serialize-fields base update)))]
    (deserialize-record base updated-record)))

(defmethod update! :coll [base table-name ids update]
  (ua/limit-map
   #(update! base table-name % update)
   ids
   (create-rate-limit-chan)))



(defn delete! [base table-name & ids]
  (ua/limit-map
   (fn [id]
     (ua/go-try-let [record (ua/<? (fetch base table-name id))
                     deleted-record (ua/<p? (.destroy
                                             ((:base base) table-name)
                                             (:_id record)))]
       (deserialize-record base deleted-record)))
   ids
   (create-rate-limit-chan)))
