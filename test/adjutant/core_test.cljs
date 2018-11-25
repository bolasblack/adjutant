(ns adjutant.core-test
  (:require
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [adjutant.core :as uc :include-macros true]))

(defn -main [])

(deftest hashify
  (let [expected {:a 2 'a 3 "a" 4 1 5}]
    (is (= expected (uc/hashify (list :a 2 'a 3 "a" 4 1 5))))
    (is (= expected (uc/hashify (list {:a 2 'a 3 "a" 4 1 5}))))))

(deftest error
  (let [err (uc/error "test error msg")]
    (is (.-message err) "test error msg"))

  (let [err (uc/error "test error msg1" {:with-meta true})]
    (is (.-message err) "test error msg1")
    (is (.-data err) {:with-meta true})
    (is (ex-data err) {:with-meta true})))

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
      (is (.-data err) {:with-meta true})
      (is (ex-data err) {:with-meta true}))))

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
  (is (= (/ 4 6)
         (uc/cond-converge 3
           dec #(* %1 %2)
           (constantly 4) #(/ %2 %1)))))

(deftest def-
  (uc/def- private-var 1)
  (is (:private (meta #'private-var))))

(deftest ->tap
  (is (= 1 (-> 1 (uc/->tap inc)))))

(deftest ->>tap
  (is (= 1 (->> 1 (uc/->>tap inc)))))
