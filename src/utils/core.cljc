(ns utils.core)

(defmacro go-let [binding & body]
  (let [go-sym #?(:clj 'clojure.core.async/go
                  :cljs 'cljs.core.async.macros/go)]
    `(~go-sym (let ~binding
                ~@body))))

(defmacro go-try-let [binding & body]
  `(async-error.core/go-try
    (let ~binding
      ~@body)))
