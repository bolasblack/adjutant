(ns utils.async-test
  (:refer-clojure :exclude [map])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [pjstadig.humane-test-output]
            [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
            [cljs.core.async :as async :refer [<! >! put!]]
            [cats.monad.either :as ce]
            [utils.async :as ua :include-macros true]))




(deftest chan?
  (is (not (ua/chan? [])))
  (is (ua/chan? (async/chan))))




(deftest promise?
  (is (not (ua/promise? [])))
  (is (ua/promise? (js/Promise.resolve 1))))




(deftest error?
  (is (ua/error? (js/Error.)))
  (is (ua/error? (js/Error.) :policy :error))

  (is (not (ua/error? [] :policy :node)))
  (is (not (ua/error? [nil] :policy :node)))
  (is (ua/error? [(js/Error.)] :policy :node))
  (is (ua/error? [""] :policy :node))

  (is (not (ua/error? "" :policy :cats-either)))
  (is (not (ua/error? (ce/right 1) :policy :cats-either)))
  (is (not (ua/error? (ce/right (js/Error.)) :policy :cats-either)))
  (is (ua/error? (ce/left 1) :policy :cats-either))

  (try
    (ua/error? "" :policy :unknown)
    (is false)
    (catch js/Error err
      (is err.message "Unsupported policy: :unknown"))))




(deftest limit-map
  (ct/async
   done
   (ua/go-let [limit-chan (async/chan 5)
               last-job-info (volatile! nil)
               last-job-result (volatile! nil)
               job-ids (range 10)
               map-chan (ua/limit-map
                         #(do (vreset! last-job-info %)
                              (* 2 %))
                         job-ids
                         limit-chan)]

     (ua/go-loop []
       (let [r (<! map-chan)]
         (vreset! last-job-result r)
         (recur)))

     (is (= nil @last-job-info))
     (is (= nil @last-job-result))

     (dotimes [n 5]
       (>! limit-chan n))
     (<! (async/timeout 0))
     (is (= 4 @last-job-info))
     (is (= 8 @last-job-result))

     (<! (async/timeout 0))
     (is (= 4 @last-job-info))
     (is (= 8 @last-job-result))

     (dotimes [n 2]
       (>! limit-chan n))
     (<! (async/timeout 0))
     (is (= 6 @last-job-info))
     (is (= 12 @last-job-result))

     (done))))




(deftest pack-value
  (let [err (js/Error. "test")]
    (is (= 1 (ua/pack-value 1)))

    (is (= 1 (ua/pack-value 1 :policy :error)))
    (is (= err (ua/pack-value err :policy :error)))

    (is (= [nil 1] (ua/pack-value 1 :policy :node)))
    (is (= [nil err] (ua/pack-value err :policy :node)))

    (is (= (ce/right 1) (ua/pack-value 1 :policy :cats-either)))
    (is (= (ce/right err) (ua/pack-value err :policy :cats-either)))))




(deftest pack-error
  (let [err (js/Error. "test")]
    (let [ex (ua/pack-error 1)]
      (is (= "" (.-message ex)))
      (is (= {:reason 1} (ex-data ex))))

    (let [ex (ua/pack-error 1 :policy :error)]
      (is (= "" (.-message ex)))
      (is (= {:reason 1} (ex-data ex))))
    (let [ex (ua/pack-error err :policy :error)]
      (is (identical? ex err)))

    (is (= [1 nil] (ua/pack-error 1 :policy :node)))
    (is (= [err nil] (ua/pack-error err :policy :node)))

    (is (= (ce/left 1) (ua/pack-error 1 :policy :cats-either)))
    (is (= (ce/left err) (ua/pack-error err :policy :cats-either)))))




(deftest from-promise
  (ct/async
   done
   (ua/go
     (with-redefs [ua/pack-value #(hash-map :packed-by :val :args %&)
                   ua/pack-error #(hash-map :packed-by :err :args %&)]
       (let [c1 (ua/from-promise (js/Promise.resolve 1) :policy :node)
             c2 (ua/from-promise (js/Promise.reject 2) :policy :cats-either)]
         (is (= {:packed-by :val :args '(1 :policy :node)} (<! c1)))
         (is (= {:packed-by :err :args '(2 :policy :cats-either)} (<! c2)))))
     (done))))




(defn- close-to? [expected actual &
                  {:keys [deviate]
                   :or {deviate 100}}]
  (some #(= actual %)
        (range (- expected deviate)
               (+ expected deviate))))

(defn- create-chan [duration & args]
  (let [chan (async/chan)]
    (go (<! (async/timeout duration))
        (>! chan (into [1] args))
        (<! (async/timeout duration))
        (>! chan (into [2] args))
        (<! (async/timeout duration))
        (>! chan (into [3] args))
        (async/close! chan))
    chan))

(deftest wait-multiple-chan
  (ct/async
   done
   (ua/go-let
     [start (js/Date.now)
      chan (async/map
            #(conj %& (- (js/Date.now) start))
            [(create-chan 100 :a1 :a2)
             (create-chan 200 :b1 :b2)])

      d1 (<! chan)
      _ (is (close-to? 200 (first d1)))
      _ (is (= (next d1)
               '([1 :a1 :a2]
                 [1 :b1 :b2])))

      d2 (<! chan)
      _ (is (close-to? 400 (first d2)))
      _ (is (= (next d2)
               '([2 :a1 :a2]
                 [2 :b1 :b2])))

      d3 (<! chan)
      _ (is (close-to? 600 (first d3)))
      _ (is (= (next d3)
               '([3 :a1 :a2]
                 [3 :b1 :b2])))]

     (done))))




(deftest go-try-test
  (ct/async
   done
   (ua/go-let [e (ex-info "foo" {})
               ch (async/chan)]
     (put! ch e)
     (is (= e (<! (ua/go-try
                   (ua/<? ch)
                   :invalid-resp))))
     (done))))

(deftest go-try-test-with-non-standard-error
  (ct/async
   done
   (ua/go-let [e [123 nil]
               ch (async/chan)]
     (put! ch e)
     (is (= e (<! (ua/go-try
                   (ua/<? ch :policy :node)
                   :invalid-resp))))
     (done))))




(defn read-both [ch-a ch-b]
  (ua/go-try
   (let [a (ua/<? ch-a)
         b (ua/<? ch-b)]
     [a b])))

(deftest read-both-test-1
  (ct/async
   done
   (ua/go-let [e (ex-info "foo" {})
               ch-a (async/chan)
               ch-b (async/chan)]
     (put! ch-a e)
     (is (= e (<! (read-both ch-a ch-b))))
     (done))))

(deftest read-both-test-2
  (ct/async
   done
   (ua/go-let [e (ex-info "foo" {})
               ch-a (async/chan)
               ch-b (async/chan)]
     (put! ch-a 1)
     (put! ch-b e)
     (is (= e (<! (read-both ch-a ch-b))))
     (done))))

(deftest read-both-test-3
  (ct/async
   done
   (ua/go-let [e (ex-info "foo" {})
               ch-a (async/chan)
               ch-b (async/chan)]
     (put! ch-a e)
     (put! ch-b 1)
     (read-both ch-a ch-b)
     (is (= 1 (<! ch-b)))
     (done))))




(deftest <<!
  (ct/async
   done
   (ua/go-let [chan1 (go (go (go 1)))
               chan2 (go 1)
               chan3 1]
     (is (= 1 (ua/<<! chan1)))
     (is (= 1 (ua/<<! chan2)))
     (is (= 1 (ua/<<! chan3)))
     (done))))
