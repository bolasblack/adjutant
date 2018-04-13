(ns utils.seq-test
  (:refer-clojure :exclude [remove])
  (:require [pjstadig.humane-test-output]
            [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
            [utils.seq :as useq]))

(deftest remove
  (is (= [0 2 3 4] (useq/remove (range 5) 1)))
  (is (= [0 2 4] (useq/remove (range 5) 1 3)))
  (is (= [0 2 3 4] (useq/remove (range 5) 1 6))))

(deftest js->seq
  (let [a #js {:a 1}
        b #js {:b 1}
        c #js {:c 1}]
    (is (= [a b c] (useq/js->seq #js [a b c])))
    (is (= [a b c] (useq/js->seq #js [a b c] [])))
    (is (= (list c b a) (useq/js->seq #js [a b c] '())))
    (is (= #{c a b} (useq/js->seq #js [a b c] #{})))))
