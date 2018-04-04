(ns utils.airtable-test
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [utils.airtable :as a]))

(deftest formula
  (is (= "AND(FIND(\"hello\", world), FIND(\"nice\", job))"
         (a/formula {:world "hello" :job "nice"})))
  (is (= "AND(FIND(\"hello\", world), FIND(\"nice\", job))"
         (a/formula :world "hello" :job "nice")))
  (is (= "AND(OR({id}=1, {id}=2, FIND(\"3\", id)), AND({id1}=1, {id1}=2, FIND(\"3\", id1)))"
         (a/formula :id [:or 1 2 "3"]
                    :id1 [:and 1 2 "3"]))))
