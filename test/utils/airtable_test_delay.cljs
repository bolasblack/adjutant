(ns utils.airtable-test-delay
  (:refer-clojure :exclude [uuid])
  (:require
   ["dotenv/config"]
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   ["uuid/v1" :as uuid]
   [utils.airtable :as a]
   [utils.core :refer-macros [error?]]
   [utils.async :as ua :refer-macros [<! <?]]
   [utils.string :as us]))

(def base (a/base :base-id js/process.env.AIRTABLE_BASE_ID
                  :start-case-fields [:single-line-text]
                  :first-upper-fields [:long-text]
                  :upper-fields [:url :id]
                  :field-name-serializer #(if (= % :checkbox?)
                                            (us/sentence-case % :keep-pred? true)
                                            %)))

(def table-name "Main Test Table")

(def default-record
  {:single-line-text "Single Line Text"
   :long-text "multiline\ntext"
   :checkbox? true
   :multiple-select ["2" "3"]
   :single-select "2"
   :date (.toJSON (js/Date.))
   :phone-number "+8618775654739"
   :email "airtable@example.com"
   :url "https://google.com"
   :number 11
   :currency 22
   :percent 33
   :rating 4
   :barcode {:text "hello barcode"}})




(deftest insert!-single
  (ct/async
   done
   (ua/go-let
     [id
      (uuid)

      res
      (<! (ua/go-try-let [res1 (<? (a/insert! base table-name (assoc default-record :id id)))
                          res2 (<? (a/fetch base table-name (:_id res1)))]
            [(:_id res1) res1 res2]))]
     (if (error? res)
       (do
         (prn "reason" (ex-data res))
         (prn "error" res)
         (js/console.error res)
         (is (not (error? res))))
       (let [[_id res1 res2] res
             expected-data (assoc default-record :_id _id :id id)]
         (is (= expected-data res1))
         (is (= expected-data res2))))
     (done))))

(deftest insert!-coll
  (ct/async
   done
   (ua/go-let
     [id1 (uuid)
      id2 (uuid)
      id3 (uuid)

      res
      (<! (ua/go-try [(<? (a/insert! base table-name (assoc default-record :id id1)))
                      (<? (a/insert! base table-name (assoc default-record :id id2)))
                      (<? (a/insert! base table-name (assoc default-record :id id3)))
                      (<? (a/filter base table-name {:id [:or id1 id2 id3]}))]))]
     (if (error? res)
       (do
         (prn "reason" (ex-data res))
         (prn "error" res)
         (js/console.error res)
         (is (not (error? res))))
       (do
         (is (= (assoc default-record :_id (:_id (nth res 0)) :id id1)
                (nth res 0)))
         (is (= (assoc default-record :_id (:_id (nth res 1)) :id id2)
                (nth res 1)))
         (is (= (assoc default-record :_id (:_id (nth res 2)) :id id3)
                (nth res 2)))))
     (done))))