(ns utils.aws.lambda
  (:require [clojure.core.async :refer [<!]]
            [utils.async :as ua :include-macros true]
            [utils.core :as uc :include-macros true]))

#?(:cljs (defn wrap-handler [f]
           (fn [event context callback]
             (ua/go-let [resp (ua/<<! (ua/go-try-let [resp (f event context)]
                                        (cond (ua/promise? resp) (ua/from-promise resp)
                                              (ua/chan? resp) resp
                                              :else (ua/pack-value resp))))]
               (if (uc/error? resp)
                 (callback resp)
                 (callback nil resp))))))

#?(:clj (defmacro defhandler [& body]
          `(wrap-handler (~'fn ~@body))))
