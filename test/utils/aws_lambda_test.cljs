(ns utils.aws-lambda-test
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [pjstadig.humane-test-output]
   [cljs.core.async :as async :refer [<! >!]]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [utils.core :as uc :include-macros true]
   [utils.async :as ua :include-macros true]
   [utils.aws.lambda :as lambda :include-macros true]))

(deftest wrap-handler
  (ct/async
   done
   (ua/go-let [event (js/Object.)
               context (js/Object.)
               fake-data (js/Object.)
               fake-error (uc/error "test error")

               promise-resolve-chan
               (async/chan)
               promise-resolve-resp
               ((lambda/wrap-handler
                 (fn [event context]
                   (async/put! promise-resolve-chan [event context])
                   (js/Promise.resolve fake-data)))
                event context
                (fn [err data]
                  (async/put! promise-resolve-chan [err data])))

               promise-reject-chan
               (async/chan)
               promise-reject-resp
               ((lambda/wrap-handler
                 (fn [event context]
                   (async/put! promise-reject-chan [event context])
                   (js/Promise.reject fake-error)))
                event context
                (fn [err data]
                  (async/put! promise-reject-chan [err data])))

               chan-normal-chan
               (async/chan)
               chan-normal-resp
               ((lambda/wrap-handler
                 (fn [event context]
                   (async/put! chan-normal-chan [event context])
                   (go fake-data)))
                event context
                (fn [err data]
                  (async/put! chan-normal-chan [err data])))

               chan-error-chan
               (async/chan)
               chan-error-resp
               ((lambda/wrap-handler
                 (fn [event context]
                   (async/put! chan-error-chan [event context])
                   (go fake-error)))
                event context
                (fn [err data]
                  (async/put! chan-error-chan [err data])))

               other-chan
               (async/chan)
               other-resp
               ((lambda/wrap-handler
                 (fn [event context]
                   (async/put! other-chan [event context])
                   fake-data))
                event context
                (fn [err data]
                  (async/put! other-chan [err data])))

               combined-chan (async/map vector [promise-resolve-chan promise-reject-chan
                                                chan-normal-chan chan-error-chan
                                                other-chan])

               [[promise-resolve-event promise-resolve-context]
                [promise-reject-event promise-reject-context]
                [chan-normal-event chan-normal-context]
                [chan-error-event chan-error-context]
                [other-event other-context]]
               (<! combined-chan)

               [[promise-resolve-err promise-resolve-data]
                [promise-reject-err promise-reject-data]
                [chan-normal-err chan-normal-data]
                [chan-error-err chan-error-data]
                [other-err other-data]]
               (<! combined-chan)]

     (is (identical? event promise-resolve-event))
     (is (identical? event promise-reject-event))
     (is (identical? event chan-normal-event))
     (is (identical? event chan-error-event))
     (is (identical? event other-event))

     (is (identical? context promise-resolve-context))
     (is (identical? context promise-reject-context))
     (is (identical? context chan-normal-context))
     (is (identical? context chan-error-context))
     (is (identical? context other-context))

     (is (nil? promise-resolve-err))
     (is (identical? fake-data promise-resolve-data))
     (is (nil? chan-normal-err))
     (is (identical? fake-data chan-normal-data))
     (is (nil? other-err))
     (is (identical? fake-data other-data))

     (is (identical? fake-error promise-reject-err))
     (is (nil? promise-reject-data))
     (is (identical? fake-error chan-error-err))
     (is (nil? chan-error-data))

     (done))))

(deftest defhandler
  (is (= (macroexpand '(lambda/defhandler [event context] 1))
         '(utils.aws.lambda/wrap-handler (fn [event context] 1)))))
