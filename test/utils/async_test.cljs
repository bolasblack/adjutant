(ns utils.async-test
  (:refer-clojure :exclude [map])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [pjstadig.humane-test-output]
            [goog.object :as go]
            [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
            [cljs.core.async :as async :refer [<! >! put!]]
            [cats.monad.either :as ce]
            [utils.core :as uc :include-macros true]
            [utils.async :as ua :include-macros true]
            ["lodash.isequal" :as js-equal]))

(deftest chan?
  (is (not (ua/chan? [])))
  (is (ua/chan? (async/chan))))




(deftest promise?
  (is (not (ua/promise? [])))
  (is (ua/promise? (js/Promise.resolve 1))))




(deftest error?
  (is (not (ua/error? 1)))
  (is (ua/error? (js/Error.)))
  (is (ua/error? (js/Error.) :policy nil))
  (is (ua/error? (js/Error.) :policy :error))
  (is (ua/error? (js/Error.) {:policy :error}))

  (is (not (ua/error? [] :policy :node)))
  (is (not (ua/error? [nil] :policy :node)))
  (is (ua/error? [(js/Error.)] :policy :node))
  (is (ua/error? [""] :policy :node))
  (is (ua/error? [""] {:policy :node}))

  (is (not (ua/error? "" :policy :cats-either)))
  (is (not (ua/error? (ce/right 1) :policy :cats-either)))
  (is (not (ua/error? (ce/right (js/Error.)) :policy :cats-either)))
  (is (ua/error? (ce/left 1) :policy :cats-either))
  (is (ua/error? (ce/left 1) {:policy :cats-either}))

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
    (is (= {:utils.async/packed-value? true
            :value nil}
           (ua/pack-value nil)))

    (is (= 1 (ua/pack-value 1)))
    (is (= 1 (ua/pack-value 1 :policy nil)))
    (is (= 1 (ua/pack-value 1 :policy :error)))
    (is (= err (ua/pack-value err :policy :error)))

    (is (= [nil 1] (ua/pack-value 1 {:policy :node})))
    (is (= [nil 1] (ua/pack-value 1 :policy :node)))
    (is (= [nil err] (ua/pack-value err :policy :node)))

    (is (= (ce/right 1) (ua/pack-value 1 :policy :cats-either)))
    (is (= (ce/right err) (ua/pack-value err :policy :cats-either)))))




(deftest unpack-value
  (let [err (js/Error. "test")]
    (is (= nil (ua/unpack-value (ua/pack-value nil))))

    (is (= 1 (ua/unpack-value 1)))
    (is (= nil (ua/unpack-value nil)))
    (is (= 1 (ua/unpack-value 1 :policy nil)))
    (is (= 1 (ua/unpack-value 1 :policy :error)))
    (is (= err (ua/unpack-value err :policy :error)))
    (is (= nil (ua/unpack-value nil :policy :error)))

    (is (= 1 (ua/unpack-value [nil 1] {:policy :node})))
    (is (= 1 (ua/unpack-value [nil 1] :policy :node)))
    (is (= err (ua/unpack-value [nil err] :policy :node)))
    (is (= nil (ua/unpack-value [1 nil] :policy :node)))
    (is (= nil (ua/unpack-value [1] :policy :node)))
    (is (= nil (ua/unpack-value nil :policy :node)))

    (is (= 1 (ua/unpack-value (ce/right 1) :policy :cats-either)))
    (is (= err (ua/unpack-value (ce/right err) :policy :cats-either)))
    (is (= nil (ua/unpack-value (ce/left 1) :policy :cats-either)))
    (is (= nil (ua/unpack-value nil :policy :cats-either)))))




(deftest pack-error
  (let [err (js/Error. "test")]
    (let [ex (ua/pack-error 1)]
      (is (= "1" (.-message ex)))
      (is (= {:reason 1, :utils.async/packed-error? true} (ex-data ex))))

    (let [ex (ua/pack-error 1 :policy :error)]
      (is (= "1" (.-message ex)))
      (is (= {:reason 1, :utils.async/packed-error? true} (ex-data ex))))
    (let [ex (ua/pack-error err :policy nil)]
      (is (identical? ex err)))
    (let [ex (ua/pack-error err :policy :error)]
      (is (identical? ex err)))

    (is (= [1 nil] (ua/pack-error 1 {:policy :node})))
    (is (= [1 nil] (ua/pack-error 1 :policy :node)))
    (is (= [err nil] (ua/pack-error err :policy :node)))

    (is (= (ce/left 1) (ua/pack-error 1 :policy :cats-either)))
    (is (= (ce/left err) (ua/pack-error err :policy :cats-either)))))




(deftest unpack-error
  (let [err (js/Error. "test")]
    (is (= 1 (ua/unpack-error (ua/pack-error 1))))
    (is (= err (ua/unpack-error err)))
    (is (= nil (ua/unpack-error nil)))
    (is (= err (ua/unpack-error err :policy nil)))
    (is (= 1 (ua/unpack-error (ua/pack-error 1) :policy :error)))
    (is (= err (ua/unpack-error err :policy :error)))
    (is (= nil (ua/unpack-error nil :policy :error)))

    (is (= 1 (ua/unpack-error [1 nil] {:policy :node})))
    (is (= 1 (ua/unpack-error [1 nil] :policy :node)))
    (is (= 1 (ua/unpack-error [1] :policy :node)))
    (is (= err (ua/unpack-error [err nil] :policy :node)))
    (is (= err (ua/unpack-error [err] :policy :node)))
    (is (= nil (ua/unpack-error [nil 1] :policy :node)))
    (is (= nil (ua/unpack-error [] :policy :node)))
    (is (= nil (ua/unpack-error nil :policy :node)))

    (is (= 1 (ua/unpack-error (ce/left 1) :policy :cats-either)))
    (is (= err (ua/unpack-error (ce/left err) :policy :cats-either)))
    (is (= nil (ua/unpack-error (ce/right err) :policy :cats-either)))
    (is (= nil (ua/unpack-error nil :policy :cats-either)))))




(deftest packed-error?
  (is (not (ua/packed-error? 1)))
  (is (not (ua/packed-error? (js/Error.))))
  (is (ua/packed-error? (ua/pack-error 1))))




(deftest promise->chan
  (ct/async
   done
   (ua/go-let [fake-error (js/Error. "fake error")
               r1 (<! (ua/promise->chan (js/Promise.resolve 1)))
               r2 (<! (ua/promise->chan (js/Promise.reject 2)))
               r3 (<! (ua/promise->chan (js/Promise.reject fake-error)))
               r4 (<! (ua/promise->chan (js/Promise.resolve nil)))]
     (is (= 1 r1))
     (is (= (ex-data (ua/pack-error 2)) (ex-data r2)))
     (is (= fake-error r3))
     (is (= (ua/pack-value nil) r4))
     (done))))

(deftest chan->promise
  (ct/async
   done
   (let [fake-error (js/Error. "fake error")
         resolve-result (fn [promise]
                          (.then
                           promise
                           (fn [val] {:type :resolve :val val})
                           (fn [val] {:type :reject :val val})))
         ps [(resolve-result (ua/chan->promise (go 1)))
             (resolve-result (ua/chan->promise (go fake-error)))
             (resolve-result (ua/chan->promise (go 1) :policy :error))
             (resolve-result (ua/chan->promise (go fake-error) :policy :error))
             (resolve-result (ua/chan->promise (go [nil 1]) :policy :node))
             (resolve-result (ua/chan->promise (go [1 nil]) :policy :node))
             (resolve-result (ua/chan->promise (go (ce/right 1)) :policy :cats-either))
             (resolve-result (ua/chan->promise (go (ce/left 1)) :policy :cats-either))]
         final-promise (js/Promise.all (clj->js ps))]
     (.then
      final-promise
      (fn [rs]
        (is (= [{:type :resolve :val 1}
                {:type :reject :val fake-error}
                {:type :resolve :val 1}
                {:type :reject :val fake-error}
                {:type :resolve :val 1}
                {:type :reject :val 1}
                {:type :resolve :val 1}
                {:type :reject :val 1}]
               (js->clj rs)))
        (done))
      (fn [err]
        (is false)
        (done))))))

(deftest <p!
  (let [fake-error (js/Error.)
        resp (macroexpand-1 '(ua/<p! promise))]
    (is (= '(utils.async/<! (utils.async/promise->chan promise))
           resp))))

(deftest <p?
  (let [fake-error (js/Error.)
        r1 (macroexpand-1 '(ua/<p? promise))
        r2 (macroexpand-1 '(ua/<p? promise :policy :node))]
    (is (= '(utils.async/<? (utils.async/promise->chan promise))
           r1))
    (is (= '(utils.async/<? (utils.async/promise->chan promise) :policy :node)
           r2))))



(defn- is-async-iterator [obj]
  (let [res-fn (go/get obj js/Symbol.asyncIterator)
        res (res-fn)
        _ (is (fn? res-fn))
        _ (is (= res obj))
        _ (is (fn? (.-next obj)))]))

(defn- next-async-iterator [next done value]
  (ua/go-let [_ (is (ua/promise? next))
              res (ua/<p! next)
              _ (is (= done (.-done res)))
              _ (is (js-equal value (.-value res)))]))

(deftest chan->async-iterator
  (ct/async
   done
   (ua/go-let [iterator (ua/chan->async-iterator (async/to-chan (range 3)))
               _ (is-async-iterator iterator)
               _ (<! (next-async-iterator (.next iterator) false 0))
               _ (<! (next-async-iterator (.next iterator) false 1))
               _ (<! (next-async-iterator (.next iterator) false 2))
               _ (<! (next-async-iterator (.next iterator) true nil))]
     (done))))

(deftest chan->async-iterator--convert-to-js
  (ct/async
   done
   (ua/go-let [iterator (ua/chan->async-iterator
                         (async/to-chan
                          [{:a 1}
                           {:a 2}])
                         :convert-to-js true)
               _ (is-async-iterator iterator)
               _ (<! (next-async-iterator (.next iterator) false #js {:a 1}))
               _ (<! (next-async-iterator (.next iterator) false #js {:a 2}))
               _ (<! (next-async-iterator (.next iterator) true js/undefined))]
     (done))))

(deftest chan->async-iterator--with-error
  (ct/async
   done
   (ua/go-let [fake-error (js/Error. "test error")
               iterator (ua/chan->async-iterator (ua/go fake-error) :policy :error)
               _ (is-async-iterator iterator)

               next (.next iterator)
               _ (is (ua/promise? next))]
     (.then
      next
      (fn [data]
        (is false)
        (done))
      (fn [err]
        (go (is (= err fake-error))
            (<! (next-async-iterator (.next iterator) true nil))
            (done)))))))




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
     (ua/go-try
      (try
        (ua/<? ch)
        (catch js/Error err
          (is (= e err)))))
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
     (put! ch e)
     (ua/go-try
      (try
        (ua/<? ch :policy :node)
        (catch js/Error err
          (is (= {:utils.async/packed-error? true
                  :reason e}
                 (ex-data err))))))
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




(deftest flat-chan
  (ct/async
   done
   (ua/go-let [fake-error (js/Error. "flat-chan error")
               chan1 (go (go (go 1)))
               chan2 (go 1)
               chan3 1
               chan4 (go (go fake-error))]
     (is (= 1 (<! (ua/flat-chan chan1))))
     (is (= 1 (<! (ua/flat-chan chan2))))
     (is (= 1 (<! (ua/flat-chan chan3))))
     (is (= fake-error (<! (ua/flat-chan chan4))))
     (done))))

(deftest <<!
  (let [res (macroexpand-1 '(ua/<<! chan))]
    (is (= '(utils.async/<! (utils.async/flat-chan chan))
           res))))

(deftest <<?
  (let [r1 (macroexpand-1 '(ua/<<? chan))
        r2 (macroexpand-1 '(ua/<<? chan :policy :node))]
    (is (= '(utils.async/<? (utils.async/flat-chan chan))
           r1))
    (is (= '(utils.async/<? (utils.async/flat-chan chan) :policy :node)
           r2))))




(deftest chan->vec
  (ct/async
   done
   (ua/go-let [chan (async/to-chan [1 2 3])
               resp (ua/<! (async/into '() chan))]
     (is (= '(3 2 1) resp))
     (done))))




(deftest denodify
  (ct/async
   done
   (ua/go-let
     [fake-read-file1
      (ua/denodify
       (js* "function readFile(path, options, callback) {
const keys = Object.keys(this)
callback(null, {
  path: path,
  options: options,
  'this-bounded?': keys.length === 1 && keys[0] === 'bounded' && this.bounded,
})}")
       #js {:bounded true})

      _
      (do
        (is (= "denodified_readFile" (.-name fake-read-file1)))
        (is (= 2 (.-length fake-read-file1))))

      resp
      (js->clj (ua/<! (fake-read-file1 "/file/path.text" nil))
               :keywordize-keys true)

      _
      (do
        (is (map? resp))
        (is (= {:path "/file/path.text"
                :options nil
                :this-bounded? true}
               resp)))


      fake-error
      (js/Error. "test error")

      fake-read-file2
      (ua/denodify (fn [path options callback] (callback fake-error)))

      _
      (do
        (is (= "denodified_fn" (.-name fake-read-file2)))
        (is (= 2 (.-length fake-read-file2))))

      resp
      (ua/<! (fake-read-file2 "/file/path.text" nil))

      _
      (do
        (is (uc/error? resp))
        (is (= fake-error resp)))]
     (done))))

(deftest denodify..
  (ct/async
   done
   (do
     (is (= (macroexpand-1 '(ua/denodify.. js/fs.readFile))
            '((utils.async/denodify js/fs.readFile))))
     (is (= (macroexpand-1 '(ua/denodify.. js/fs.readFile "foo"))
            '((utils.async/denodify js/fs.readFile) "foo")))

     (is (= (macroexpand-1 '(ua/denodify.. ctx -a))
            '((utils.async/denodify (.. ctx -a) ctx))))
     (is (= (macroexpand-1 '(ua/denodify.. ctx -a "foo"))
            '((utils.async/denodify (.. ctx -a) ctx) "foo")))

     (is (= (macroexpand-1 '(ua/denodify.. ctx -a -b -c))
            '((utils.async/denodify (.. ctx -a -b -c) (.. ctx -a -b)))))
     (is (= (macroexpand-1 '(ua/denodify.. ctx -a -b -c "foo"))
            '((utils.async/denodify (.. ctx -a -b -c) (.. ctx -a -b)) "foo")))

     (let [obj #js{:a #js{:b nil}}]
       (set! (.-b (.-a obj))
             (fn [path callback]
               (callback nil {:path path
                              :this-bounded? (identical? (js* "this")
                                                         (.-a obj))})))
       (ua/go-let
         [resp1 (ua/<! (ua/denodify..
                        (fn [path callback]
                          (callback nil {:path path}))
                        "test path"))
          _ (is (= {:path "test path"} resp1))
          resp2 (ua/<! (ua/denodify.. obj -a -b "test path"))
          _ (is (= {:path "test path" :this-bounded? true} resp2))]
         (done))))))
