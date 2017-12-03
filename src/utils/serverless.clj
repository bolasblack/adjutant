(ns utils.serverless)

(defmacro defhandler [& body]
  `(let [handler# (~'fn ~@body)]
     (~'require-macros '[cljs.core.async.macros])
     (~'require '[cljs.core.async])
     (defn ~'-main [event# context# callback#]
       (cljs.core.async.macros/go
         (let [resp# (cljs.core.async/<! (handler# event# context#))]
           (if (and (vector? resp#) (= 2 (count resp#)))
             (callback# (first resp#) (nth 1 resp#))
             (callback# nil resp#)))))))
