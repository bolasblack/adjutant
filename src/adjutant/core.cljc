(ns adjutant.core
  #?(:cljs (:require-macros
            [adjutant.core :refer [if-cljs error! assert-args defmacro- def-]]
            [clojure.core.incubator :as incu]))
  (:require
   #?@(:clj [[clojure.core.incubator :as incu]])))

(defn hashify
  "Transform arguments to hash-map

  Example:

  (defn my-func [& inputs]
    (let [opts (hashify inputs)]
      opts))

  (my-func {:a 1}) #=> {:a 1}
  (my-func :a 1) #=> {:a 1}"
  [args]
  (if (and (= 1 (count args))
           (map? (first args)))
    (first args)
    (apply hash-map args)))

#?(:clj
   (defn cljs-env?
     "Take the &env from a macro, and tell whether we are expanding into cljs."
     [env]
     (boolean (:ns env))))

#?(:clj
   (defmacro if-cljs
     "Return then if we are generating cljs code and else for Clojure code.
  https://github.com/plumatic/schema/blob/012396d62842af8191dbd65166746aa72996d4f1/src/clj/schema/macros.clj#L10-L19
  https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
     [then else]
     (if (cljs-env? &env) then else)))

(defn error
  "Generate a cross-platform exception"
  ([s]
   #?(:cljs (js/Error. s)
      :clj (RuntimeException. s)))
  ([s m]
   #?(:cljs (ex-info s m)
      :clj (ex-info s m))))

#?(:clj
   (defmacro error!
     "Generate a cross-platform exception appropriate to the macroexpansion context
  https://github.com/plumatic/schema/blob/012396d62842af8191dbd65166746aa72996d4f1/src/clj/schema/macros.clj#L33-L43"
     ([s]
      `(if-cljs
        (throw (error ~s))
        (throw (error ~(if (seq? s)
                         (with-meta s `{:tag java.lang.String})
                         s)))))
     ([s m]
      `(if-cljs
        (throw (error ~s ~m))
        (throw (error ~(if (seq? s)
                         (with-meta s `{:tag java.lang.String})
                         s) ~m))))))

(defn error?
  "Detect obj is an error"
  [obj]
  (instance?
   (if-cljs js/Error Exception)
   obj))

(defn- cond-converge*
  [initial-val [[pred processer] & rest-pairs]]
  (if-let [pred-res (pred initial-val)]
    (let [process-res (processer initial-val pred-res)]
      (if rest-pairs
        (recur process-res rest-pairs)
        process-res))
    initial-val))

(defn cond-converge
  "Example:

  (cond-converge initial-val
    pred1-always-return-3 +
    pred2-always-return-false +)

  Equals:

  (if-let [test1-result (fn1-always-return-3 initial-val)]
    (let [step1-result (+ initial-val test1-result)]
      (if-let [test2-result (pred2-always-return-false step1-result)]
        (let [step2-result (+ step1-result test2-result)]
          step2-result)
        step1-result))
    initial-val)"
  [initial-val & clauses]
  (assert (even? (count clauses)))
  (let [clause-pairs (partition 2 clauses)]
    (cond-converge* initial-val clause-pairs)))

#?(:clj
   (defmacro assert-args
     "Help assert arguments in macros

  Source from:
  https://github.com/clojure/clojure/blob/f27f6de232a5f024613cd235b7fa2c38b40763db/src/clj/clojure/core.clj#L1824

  Example:
  https://github.com/clojure/clojure/blob/f27f6de232a5f024613cd235b7fa2c38b40763db/src/clj/clojure/core.clj#L1842"
     [& pairs]
     `(do (when-not ~(first pairs)
            (throw (IllegalArgumentException.
                    (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
          ~(let [more (nnext pairs)]
             (when more)))))

#?(:clj
   (do
     (defmacro defmacro-
       [name & decls]
       `(incu/defmacro- ~name ~@decls))
     (alter-meta! #'defmacro- assoc :doc (:doc (meta #'incu/defmacro-)))))

#?(:clj
   (defmacro def-
     "Same as def but yields a private definition"
     [name & decls]
     (list* `def (with-meta name (assoc (meta name) :private true)) decls)))

(defn ->tap
  "(-> 1
       (->tap pr)
       (is= 1)"
  [v f]
  (f v)
  v)

(defn ->>tap
  "(->> 1
       (->>tap pr)
       (is= 1)"
  [f v]
  (f v)
  v)
