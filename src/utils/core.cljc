(ns utils.core)

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

#?(:clj
   (defmacro error!
     "Generate a cross-platform exception appropriate to the macroexpansion context
  https://github.com/plumatic/schema/blob/012396d62842af8191dbd65166746aa72996d4f1/src/clj/schema/macros.clj#L33-L43"
     ([s]
      `(if-cljs
        (throw (js/Error. ~s))
        (throw (RuntimeException. ~(if (seq? s)
                                     (with-meta s `{:tag java.lang.String})
                                     s)))))
     ([s m]
      `(if-cljs
        (throw (ex-info ~s ~m))
        (throw (clojure.lang.ExceptionInfo. ~(if (seq? s)
                                               (with-meta s `{:tag java.lang.String})
                                               s) ~m))))))

#?(:clj
   (defmacro error? [obj]
     `(instance?
       (if-cljs js/Error Exception)
       ~obj)))

#?(:clj
   (defmacro cond-converge
     "Example:

  (macroexpand-1
   '(cond-converge 1
      (constantly 3)
      (fn [const-3 prev-val]
        (+ const-3 prev-val))))

  # =>

  (let [G__4572 (identity 1)
        G__4572 (let [test__4547__auto__ (constantly 3)
                      step__4548__auto__ (fn [const-3 prev-val] (+ const-3 prev-val))
                      ___4549__auto__ (assert (and (fn? test__4547__auto__)
                                                   (fn? step__4548__auto__)))
                      test-result__4550__auto__ (test__4547__auto__ G__4572)]
                  (if test-result__4550__auto__
                    (step__4548__auto__ test-result__4550__auto__ G__4572)
                    G__4572))]
    G__4572)"
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
