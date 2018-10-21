(ns utils.string
  (:require
   [clojure.core.strint :as strint]))

(defmacro << [& strings]
  `(strint/<< ~@strings))

(alter-meta! #'<< assoc :doc (:doc (meta #'strint/<<)))
