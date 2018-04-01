(ns utils.airtable
  (:refer-clojure :exclude [filter])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require ["airtable" :as Airtable]
            [cljs.core.async :as async :refer [>! <! put! take! close!]]
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



(defn formula [& opt-coll]
  (let [safe-str (fn safe-str [a]
                   (cond-> a
                     (keyword? a) name
                     (symbol? a) name
                     true str))
        gen-find-subformula (fn gen-find-subformula [pair]
                              (str "FIND(\"" (last pair) "\", {" (safe-str (first pair)) "})"))
        conditions (if (= 1 (count opt-coll))
                     (->> (first opt-coll)
                          (into [])
                          flatten)
                     opt-coll)
        opt-pairs (partition 2 conditions)
        subformulas (map gen-find-subformula opt-pairs)]
    (str "AND(" (str/join ", " subformulas) ")")))



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



(defn filter [base table-name & {:keys [limit]
                                 :or {limit js/Infinity}
                                 :as opts}]
  (let [chan (async/chan)
        sent-count-ref (atom 0)
        conditon (clj->js (dissoc opts :limit))]
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



(defmulti insert! (fn [base table-name records]
                    (if (map? records) :single :coll)))

(defmethod insert! :single [base table-name record]
  (ua/go-try-let [record (ua/<p? (.create
                                  ((:base base) table-name)
                                  (serialize-fields base record)))]
    (deserialize-record base record)))

(defmethod insert! :coll [base table-name records]
  (ua/limit-map
   (fn [record]
     (println "create record" record)
     (ua/go-try-let [resp (ua/<? (insert! base table-name record))]
       (println "create record finished, " record)
       resp))
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
   (fn [id]
     (println "patch record" id)
     (ua/go-try-let [resp (ua/<? (update! base table-name id update))]
       (println "patch record finished, " id)
       resp))
   ids
   (create-rate-limit-chan)))



(defmulti delete! (fn [base table-name ids]
                    (if (coll? ids) :coll :single)))

(defmethod delete! :single [base table-name id]
  (ua/go-try-let [record (ua/<? (fetch base table-name id))
                  deleted-record (ua/<p? (.destroy
                                          ((:base base) table-name)
                                          (:_id record)))]
    (deserialize-record base deleted-record)))

(defmethod delete! :coll [base table-name ids]
  (ua/limit-map
   (fn [id]
     (println "delete record" id)
     (ua/go-try-let [resp (ua/<? (delete! base table-name id))]
       (println "delete record finished, " id)
       resp))
   ids
   (create-rate-limit-chan)))
