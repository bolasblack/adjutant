(ns utils.core)

(defmacro go-let [binding & body]
  `(~'go (~'let ~binding
          ~@body)))
