(ns utils.core-test
  (:require [pjstadig.humane-test-output]
            [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
            [utils.core :as uc :include-macros true]))

(deftest error!
  (try
    (uc/error! "test error msg")
    (is false)
    (catch js/Error err
      (is (.-message err) "test error msg")))

  (try
    (uc/error! "test error msg1" {:with-meta true})
    (is false)
    (catch js/Error err
      (is (.-message err) "test error msg1")
      (is (.-data err) {:with-meta true}))))

(deftest error?
  (is (not (uc/error? 1)))
  (is (uc/error? (js/Error.)))
  (is (uc/error? (js/TypeError.)))
  (try
    (uc/error! "test")
    (catch js/Error err
      (is (uc/error? err))))
  (try
    (uc/error! "test" {:with-data true})
    (catch js/Error err
      (is (uc/error? err)))))

(deftest cond-converge
  (is (= 1.5
         (uc/cond-converge 3
           dec #(* %1 %2)
           (constantly 4) #(/ %2 %1)))))