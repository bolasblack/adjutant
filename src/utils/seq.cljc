(ns utils.seq
  (:refer-clojure :exclude [remove]))

(defn remove [coll & idx]
  (let [idx-set (set idx)]
    (keep-indexed #(if-not (contains? idx-set %1) %2) coll)))

(defn js->seq
  ([ary]
   (js->seq ary []))
  ([ary initial]
   {:pre [(js/Array.isArray ary)]}
   (.reduce ary #(conj %1 %2) initial)))
