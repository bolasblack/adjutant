(ns utils.cats-either-test
  (:refer-clojure :exclude [flatten])
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [cats.monad.either :as ce]
   [utils.cats.either :as uce]))

(deftest flatten
  (is (= (ce/left 1) (uce/flatten [(ce/right 1) (ce/left 1) (ce/right 1)])))
  (is (= (ce/right '(1 2)) (uce/flatten [(ce/right 1) (ce/right 2)]))))
