(ns utils.async
  (:refer-clojure :exclude [map])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :as async :refer [>! <! take! close!]]
            [cats.monad.either :as ce]))

(defn chan? [a]
  (= (type (async/chan)) (type a)))

(defn map [f coll & {:keys [limit failed?]}]
  (let [limit-chan (if limit
                     (async/chan (- limit 1))
                     (async/chan))
        result-chan (async/chan)]
    (go (doseq [item coll]
          (let [operate-chan (f item)]
            (take! operate-chan
                   (fn [result]
                     (>! result-chan result)
                     (if (and (fn? failed?) (failed? result))
                       (close! limit-chan)
                       (take! limit-chan identity))))
            (>! limit-chan operate-chan))))
    result-chan))

(defn combine [chs]
  (async/map (fn [& results] (vec results)) chs))

(defn denodify [& chs]
  (async/map
   (fn [[err data]]
     (if err (ce/left err) (ce/right data)))
   chs))

(defn promise->chan [promise]
  (let [chan (async/chan)]
    (-> promise
        (.then (fn [res] (>! chan [nil res])))
        (.catch (fn [err] (>! chan [err nil]))))
    chan))

(comment
  (defn repeat-chan [n prefix]
    (let [chan (async/chan)]
      (go-loop [times 0]
        (if (> times 3)
          (async/close! chan)
          (do
            (println "repeat-chan" times prefix)
            (<! (async/timeout n))
            (>! chan (str prefix "-" times))
            (recur (+ times 1)))))
      chan))

  (let [a (repeat-chan 1000 "a")
        b (repeat-chan 2000 "b")
        c (repeat-chan 4000 "c")
        d (combine [a b c])]
    (go-loop []
      (when-let [resp (<! d)]
        (println "resp" resp)
        (recur)))))
