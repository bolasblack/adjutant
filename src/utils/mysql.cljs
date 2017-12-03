(ns utils.mysql
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require ["mysql2" :as mysql]
            ["lodash.isobjectlike" :as is-object-like]
            [cljs.core.async :as async]))

(defn- normalize-js [a]
  (cond (js/Array.isArray a) (.map a normalize-js)
        (is-object-like a) (js/Object.assign #js {} a)
        :else a))

(defn conn [& config]
  (if (= 1 (count config))
    (mysql/createConnection (first config))
    (mysql/createConnection (clj->js (apply hash-map config)))))

(defn query [conn query & args]
  (let [chan (async/chan)]
    (.query conn (clj->js query) (clj->js args)
            (fn [err _rows _fields]
              (let [rows (js->clj (normalize-js _rows) :keywordize-keys true)
                    fields (js->clj (normalize-js _fields) :keywordize-keys true)]
                (when err
                  (set! (.-query err) query)
                  (set! (.-args err) args))
                (async/put! chan [err {:rows rows :fields fields}])
                (async/close! chan))))
    chan))
