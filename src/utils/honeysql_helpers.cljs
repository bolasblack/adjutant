(ns utils.honeysql-helpers
  (:require-macros [honeysql.helpers :refer [defhelper]])
  (:require [clojure.string :as str]
            [honeysql.format :as fmt]
            [honeysql.helpers :refer [build-clause]]))

(defn- parse-column [column-name type & opts]
  (-> (apply hash-map opts)
      (into {:name column-name
             :type type})))

(defn- format-column [{:keys [name type null? default auto-inc? primary-key?]
                       :or {null? true
                            auto-inc? false
                            primary-key? false}
                       :as opts}]
  (str "`" (fmt/to-sql name) "` "
       (str (if (vector? type)
              (if (> 1 (count type))
                (fmt/to-sql (first type))
                (str (fmt/to-sql (first type)) "(" (->> (next type)
                                                        (map fmt/to-sql)
                                                        (str/join ",")) ")"))
              (fmt/to-sql type))
            " ")
       (when-not null? "NOT NULL ")
       (when (some? default) (str "DEFAULT " (fmt/to-sql default) " "))
       (when auto-inc? "AUTO_INCREMENT ")
       (when primary-key? "PRIMARY KEY ")))

(defn- format-unique-indeces [{:keys [opts] :as info}]
  (let [format-unique-index
        (fn [{:keys [name columns]}]
          (str "UNIQUE KEY `" (fmt/to-sql name) "` ("
               (->> columns
                    (map #(str "`" (fmt/to-sql %) "`"))
                    (str/join ","))
               ")"))
        idx-infos (or (:unique opts) [])
        result (map format-unique-index idx-infos)]
    result))

(defhelper create-table [m [table-name & args]]
  (let [[columns opts] (split-with (complement keyword?) args)
        opts (apply hash-map opts)]
    (assoc m :create-table {:table-name table-name
                            :columns (map #(apply parse-column %) columns)
                            :opts opts})))

(defmethod fmt/format-clause :create-table [[op v] sqlmap]
  (let [table-name (fmt/to-sql (:table-name v))
        columns-cmds (doall (map format-column (:columns v)))
        uniq-idx-cmds (format-unique-indeces v)
        result (str "CREATE TABLE IF NOT EXISTS `" table-name "` (\n"
                    (->> (concat columns-cmds uniq-idx-cmds)
                         (str/join ",\n"))
                    "\n);")]
    result))
