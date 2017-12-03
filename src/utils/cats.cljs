(ns utils.cats
  (:require [cats.monad.either :as ce]))

(defn either-flatten [eithers]
  (let [first-left (ce/first-left eithers)
        result (or first-left
                   (ce/right (mapv deref eithers)))]
    result))
