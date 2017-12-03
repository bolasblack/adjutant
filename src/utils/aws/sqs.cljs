(ns utils.aws.sqs
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! close!]]
            [utils.aws :as aws]
            ["aws-sdk" :refer [SQS]]))

(def sqs (SQS.))

(defn send-message [& params]
  (let [chan (async/chan)
        sqs-opts (->> params
                      (partition 2)
                      (map vec)
                      aws/paramify
                      clj->js)]
    (.sendMessage
     sqs
     sqs-opts
     (fn [err data]
       (go
         (>! chan (if err
                    [err nil]
                    [nil {:md5-of-message-body data.MD5OfMessageBody
                          :md5-of-message-attributes data.MD5OfMessageAttributes
                          :message-id data.MessageId
                          :sequence-number data.SequenceNumber}]))
         (close! chan))))
    chan))
