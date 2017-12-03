(ns utils.airtable)

(defmacro with-base [base-id & body]
  `(let [Airtable# (js/require "airtable")]
     (binding [utils.airtable/*airtable-db* (-> (new Airtable#)
                                                (.base ~base-id))]
       ~@body)))
