(ns utils.cats.either
  (:refer-clojure :exclude [flatten])
  (:require [cats.monad.either :as ce]))

(defn flatten [eithers]
  (or (ce/first-left eithers)
      (ce/right (map deref eithers))))
