(ns utils.seq
  (:refer-clojure :exclude [remove]))

(defn remove [coll & idx]
  (let [idx-set (set idx)]
    (keep-indexed #(if-not (contains? idx-set %1) %2) coll)))
