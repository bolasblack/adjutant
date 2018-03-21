(ns utils.airtable
  (:refer-clojure :exclude [filter])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require ["airtable" :as Airtable]
            [cljs.core.async :as async :refer [>! <! put! take! close!]]
            [utils.async :as ua]
            [utils.string :as us]
            [clojure.string :as str]))

(def RATE_LIMIT 5)

(def ^:dynamic *airtable-db* nil)

(def ^:dynamic *airtable-upper-fields* [])

(def ^:dynamic *airtable-lower-fields* [])

(def ^:dynamic *airtable-field-serializers* {})

(defn- create-rate-limit-chan []
  (let [chan (async/chan RATE_LIMIT)]
    (go-loop []
      (dotimes [n 5]
        (>! chan :start))
      (<! (async/timeout (* 1000 60)))
      (recur))
    chan))

(defn- serialize-fields [fields]
  (->> (us/start-case-keys fields)
       (us/walk-keys
        (fn [key]
          (cond
            (some #(= key %) *airtable-upper-fields*)
            (str/upper-case key)

            (some #(= key %) *airtable-lower-fields*)
            (str/lower-case key)

            :else
            (if-let [serializer (key *airtable-field-serializers*)]
              (serializer key)
              key))))
       clj->js))

(defn- gen-formula [& opt-coll]
  (let [safe-str (fn safe-str [a]
                   (cond-> a
                     (keyword? a) name
                     (symbol? a) name
                     true str))
        gen-find-subformula (fn gen-find-subformula [pair]
                              (str "FIND(\"" (last pair) "\", {" (safe-str (first pair)) "})"))
        opt-pairs (partition 2 opt-coll)
        subformulas (map gen-find-subformula opt-pairs)]
    (str "AND(" (str/join ", " subformulas) ")")))

(defn filter [table-name & {:keys [formula limit]
                            :or {limit js/Infinity}
                            :as opts}]
  (let [chan (async/chan)
        sent-count-ref (atom 0)
        conditon (cond-> {:view "Main View"}
                   formula (assoc :filterByFormula formula)
                   (not formula) (#(->> (dissoc opts :formula :limit)
                                        ((fn [opts] (apply gen-formula (flatten (into [] opts)))))
                                        (assoc % :filterByFormula)))
                   true clj->js)]
    (-> (*airtable-db* table-name)
        (.select conditon)
        (.eachPage
         (fn [records fetch-next-page]
           (.forEach
            records
            (fn [record]
              (let [sent-count (deref sent-count-ref)]
                (when (< sent-count limit)
                  (put! chan [nil (-> (.-fields record)
                                      js->clj
                                      (into {"_id" (.-id record)}))])
                  (swap! sent-count-ref #(+ % 1))))))
           (if (< (deref sent-count-ref) limit)
             (fetch-next-page)
             (close! chan)))
         (fn [err]
           (when err (put! chan [err nil]))
           (close! chan))))
    chan))

(defn all [table-name]
  (filter table-name :formula "1 = 1"))

(defn fetch [table-name id]
  (filter table-name
          :formula (str "SEARCH('" id "', {ID}) = 1")
          :limit 1))



(defmulti insert! (fn [table-name books] (if (map? books) :single :coll)))

(defmethod insert! :single [table-name book]
  (let [chan (async/chan)
        data (serialize-fields book)]
    (-> (*airtable-db* table-name)
        (.create (clj->js data)
                 (fn [err record]
                   (put! chan [err record])
                   (close! chan))))
    chan))

(defmethod insert! :coll [table-name books]
  (ua/limit-map
   (fn [book]
     (go (println "create book" book)
         (let [resp (<! (insert! table-name book))]
           (println "create book finished, " book)
           resp)))
   books
   (create-rate-limit-chan)))



(defmulti update! (fn [table-name ids update] (if (coll? ids) :coll :single)))

(defmethod update! :single [table-name id update]
  (let [chan (async/chan)]
    (go (let [record (<! (fetch table-name id))]
          (-> (*airtable-db* table-name)
              (.update
               (record "_id")
               (serialize-fields update)
               (fn [err record]
                 (put! chan [err record])
                 (close! chan))))))
    chan))

(defmethod update! :coll [table-name ids update]
  (ua/limit-map
   (fn [id]
     (go (println "patch book" id)
         (let [resp (<! (update! table-name id update))]
           (println "patch book finished, " id)
           resp)))
   ids
   (create-rate-limit-chan)))



(defmulti delete! (fn [table-name ids] (if (coll? ids) :coll :single)))

(defmethod delete! :single [table-name id]
  (let [chan (async/chan)]
    (go (let [record (<! (fetch table-name id))]
          (-> (*airtable-db* table-name)
              (.destroy
               (record "_id")
               (fn [err record]
                 (put! chan [err record])
                 (close! chan))))))
    chan))

(defmethod delete! :coll [table-name ids]
  (ua/limit-map
   (fn [id]
     (go (println "delete book" id)
         (let [resp (<! (delete! table-name id))]
           (println "delete book finished, " id)
           resp)))
   ids
   (create-rate-limit-chan)))
