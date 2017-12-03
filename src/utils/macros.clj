(ns utils.macros)

(defmacro condpipe [expr & clauses]
  (assert (even? (count clauses)))
  (let [fn-clause? (fn [expr]
                     (and (seq? expr)
                          (contains? #{'fn 'fn*} (first expr))))
        g (gensym)
        steps (map (fn [[test step]] `(if-let [test-result# ~(if (fn-clause? test)
                                                               `(~test ~g)
                                                               test)]
                                        ~(if (fn-clause? step)
                                           `(~step test-result# ~g)
                                           `(-> ~g ~step))
                                        ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(comment
  (assert (=
           (macroexpand-1 '
            (condpipe (identity 1)
                      (always true) (+ 2)
                      always #(identity 2)
                      #(always 3) #(identity 4)
                      false (fn hello [] 2)))

           '(clojure.core/let [G__1497 (identity 1)
                               G__1497 (clojure.core/if-let [test-result__1480__auto__ (always true)]
                                         (clojure.core/-> G__1497 (+ 2))
                                         G__1497)
                               G__1497 (clojure.core/if-let [test-result__1480__auto__ always]
                                         ((fn* [] (identity 2)) test-result__1479__auto__ G__1497)
                                         G__1497)
                               G__1497 (clojure.core/if-let [test-result__1480__auto__ ((fn* [] (always 3)) G__1497)]
                                         ((fn* [] (identity 4)) test-result__1479__auto__ G__1497)
                                         G__1497)]
              (clojure.core/if-let [test-result__1480__auto__ false]
                ((fn hello [] 2) test-result__1479__auto__ G__1497)
                G__1497))
           )))
