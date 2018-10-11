(ns utils.core
  #?(:cljs (:require-macros [utils.core :refer [if-cljs]])))

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

(defn error? [obj]
  (instance?
   (if-cljs js/Error Exception)
   obj))

#?(:clj
   (defmacro cond-converge
     "Example:

  (cond-converge initial-val
    pred1-always-return-3 +
    pred2-always-return-false +)

  Equals:

  (if-let [test1-result (fn1-always-return-3 initial-val)]
    (let [step1-result (+ test1-result initial-val)]
      (if-let [test2-result (pred2-always-return-false step1-result)]
        (let [step2-result (+ test2-result step1-result)]
          step2-result)
        step1-result))
    initial-val)"
     [expr & clauses]
     (assert (even? (count clauses)))
     (let [g (gensym)
           steps (map (fn [[test step]]
                        `(let [test# ~test
                               step# ~step
                               _# (assert (and (fn? test#)
                                               (fn? step#)))
                               test-result# (test# ~g)]
                           (if test-result#
                             (step# test-result# ~g)
                             ~g)))
                      (partition 2 clauses))]
       `(let [~g ~expr
              ~@(interleave (repeat g) steps)]
          ~g))))
