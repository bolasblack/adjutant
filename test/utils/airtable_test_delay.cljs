(ns utils.airtable-test-delay
  (:refer-clojure :exclude [uuid])
  (:require
   ["dotenv/config"]
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   ["uuid/v4" :as uuid]
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




(deftest insert!
  (ct/async
   done
   (ua/go-let
     [id1 (uuid)
      id2 (uuid)
      id3 (uuid)

      insert (<! (ua/chan->vec
                  (a/insert!
                   base table-name
                   (assoc default-record :id id1)
                   (assoc default-record :id id2)
                   (assoc default-record :id id3))))
      filter-res (<! (ua/chan->vec (a/filter base table-name :formula {:ID [:or id1 id2 id3]})))]

     (if (error? insert)
       (do
         (prn "reason" (ex-data insert))
         (prn "error" insert)
         (js/console.error insert)
         (is (not (error? insert))))
       (let [[insert1 insert2 insert3] insert
             filter1 (first (filter #(= id1 (:id %)) filter-res))
             filter2 (first (filter #(= id2 (:id %)) filter-res))
             filter3 (first (filter #(= id3 (:id %)) filter-res))
             expected1 (assoc default-record :_id (:_id insert1) :id id1)
             expected2 (assoc default-record :_id (:_id insert2) :id id2)
             expected3 (assoc default-record :_id (:_id insert3) :id id3)]
         (is (= 3 (count filter-res)))

         (is (= expected1 insert1))
         (is (= expected1 filter1))

         (is (= expected2 insert2))
         (is (= expected2 filter2))

         (is (= expected3 insert3))
         (is (= expected3 filter3))))
     (done))))
