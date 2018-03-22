(ns utils.honeysql-helpers-test
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [honeysql.core :as sql]
   [utils.honeysql-helpers :as usqlh]))

(deftest create-table
  (is (= [(str "CREATE TABLE IF NOT EXISTS `recordSnapshots` (\n"
               "`id` int(?) AUTO_INCREMENT PRIMARY KEY ,\n"
               "`dbId` varchar(?) ,\n"
               "`tableName` varchar(?) ,\n"
               "`recordId` varchar(?) ,\n"
               "`data` text ,\n"
               "`createdAt` datetime ,\n"
               "`updatedAt` datetime ,\n"
               "UNIQUE KEY `dbId_recordId_uniq` (`dbId`,`recordId`)"
               "\n);")
          11 255 255 255]
         (sql/format
          (usqlh/create-table
           :recordSnapshots
           [:id [:int 11] :auto-inc? true :primary-key? true]
           [:dbId [:varchar 255]]
           [:tableName [:varchar 255]]
           [:recordId [:varchar 255]]
           [:data :text]
           [:createdAt :datetime]
           [:updatedAt :datetime]
           :unique [{:name :dbId-recordId-uniq
                     :columns [:dbId :recordId]}])))))
