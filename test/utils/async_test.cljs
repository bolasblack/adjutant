(ns utils.async-test
  (:refer-clojure :exclude [map])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [pjstadig.humane-test-output]
            [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
            [cljs.core.async :as async :refer [<! >! put!]]
            [cats.monad.either :as ce]
            [utils.async :as ua :include-macros true]))

(defn- round-time [time]
  (js/Math.round (/ time 1000)))

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

(deftest chan?
  (is (not (ua/chan? [])))
  (is (ua/chan? (async/chan))))

(deftest promise?
  (is (not (ua/promise? [])))
  (is (ua/promise? (js/Promise.resolve 1))))

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

(deftest from-promise
  (ct/async
   done
   (ua/go-let [c1 (ua/from-promise (js/Promise.resolve 1))
               c2 (ua/from-promise (js/Promise.reject 1))]
     (is (= [nil 1] (<! c1)))
     (is (= [1 nil] (<! c2)))
     (done))))

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
