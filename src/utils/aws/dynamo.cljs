(ns utils.aws.dynamo
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cats.core :refer [alet]]
                   [utils.macros :refer [condpipe]])
  (:require ["aws-sdk" :refer [DynamoDB]]
            [cats.core :as cc]
            [cats.monad.either :as ce]
            [cljs.core.async :as async]
            [utils.string :as us]
            [clojure.string :as str]))

(defn- table-name-ify [s]
  (keyword (us/kebab-case s)))

(defn- list->map [l]
  (if (map? (first l))
    (first l)
    (apply hash-map l)))

(defn- str-full-contains [str1 str2]
  (if (= str1 str2) str1 nil))

(defn conn [& params]
  (DynamoDB. (list->map params)))

;; offical api

(defn list-tables [conn & {:keys [limit start-table]}])

(defn describe-table [conn table-name])

(defn create-table! [])

(defn update-table! [])

(defn delete-table! [])

(defn query! [conn & params]
  (let [chan (async/chan)]
    (println "query! params" params)
    (.query conn
            (clj->js (list->map params))
            (fn [err data]
              (async/put! chan [err (js->clj data :keywordize-keys true)])
              (async/close! chan)))
    chan))

(defn get-item! [])

(defn put-item! [conn & params]
  (let [chan (async/chan)]
    (.putItem conn
              (clj->js (list->map params))
              (fn [err data]
                (async/put! chan [err (js->clj data :keywordize-keys true)])
                (async/close! chan)))
    chan))

(defn update-item! [])

(defn delete-item! [])

;; extend api

(defn serialize-item [item]
  (letfn [(buffer? [o] (instance? js/Buffer o))
          (get-type [item]
            (condp apply [item]
              string? "S"
              number? "N"
              buffer? "B"
              #(and (seq? %) (every? string? %)) "SS"
              #(and (seq? %) (every? number? %)) "NS"
              #(and (seq? %) (every? buffer? %)) "BS"
              map? "M"
              seq? "L"
              nil? "NULL"
              boolean? "BOOL"))
          (parse-pair [[key value]]
            (let [type (get-type value)
                  value (if (= type "M")
                          (parse-item value)
                          value)]
              [key {type value}]))
          (parse-item [item]
            (->> (map parse-pair item)
                 (into {})))]
    (parse-item item)))

(defn deserialize-item [item]
  (letfn [(parse-value [type value]
            (condp = (name type)
              "S" value
              "N" (js/Number value)
              "B" (js/Buffer.from value "base64")
              "SS" (map #(parse-value "S" %) value)
              "NS" (map #(parse-value "N" %) value)
              "BS" (map #(parse-value "B" %) value)
              "M" (parse-item value)
              "L" value
              "NULL" nil
              "BOOL" value))
          (parse-pair [[key value-info]]
            (let [type (first (keys value-info))
                  value (value-info type)]
              [key (parse-value type value)]))
          (parse-item [item]
            (->> (map parse-pair item)
                 (into {})))]
    (parse-item item)))

(defn filter! [conn & rest-params]
  (let [chan (async/chan)
        params (list->map rest-params)]
    (println "filter! params" params)
    (go-loop [[err query-resp] (async/<! (query! conn params))]
      (if err
        (do (async/>! chan [err nil])
            (async/close! chan))
        (do
          (doseq [item (:Items query-resp)]
            (async/>! chan [nil item]))
          (if-let [last-evaluated-key (:LastEvaluatedKey query-resp)]
            (let [next-page-params (assoc params :ExclusiveStartKey last-evaluated-key)]
              (recur (async/<! (query! conn next-page-params))))
            (async/close! chan)))))
    chan))
