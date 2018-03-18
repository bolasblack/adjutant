(ns utils.aws.lambda
  (:require [cljs.core.async :as async :refer [<! >! close!]]
            [utils.async :as ua :include-macros true]
            [utils.core :as uc :include-macros true]))

(defn wrap-handler [f]
  (fn [event context callback]
    (let [resp (ua/go-try-let [resp (f event context)]
                 (cond (ua/promise? resp) (ua/from-promise resp)
                       (ua/chan? resp) (ua/<? resp)
                       :else (ua/pack-value resp)))]
      (if (uc/error? resp)
        (callback resp)
        (callback nil resp)))))

#?(:clj (defmacro defhandler [& body]
          `(wrap-handler (~'fn ~@body))))
