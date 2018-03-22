(ns utils.aws.sqs
  (:require [cljs.core.async :as async :refer [put! close!]]
            [utils.string :as us]
            ["aws-sdk" :refer [SQS]]))

(def sqs (SQS.))

(defn send-message [& params]
  (let [chan (async/chan)
        sqs-opts (->> params
                      (partition 2)
                      (map vec)
                      (#(us/paramify % :first-upper? true))
                      clj->js)]
    (.sendMessage
     sqs
     sqs-opts
     (fn [err data]
       (put! chan
             (or err
                 {:md5-of-message-body data.MD5OfMessageBody
                  :md5-of-message-attributes data.MD5OfMessageAttributes
                  :message-id data.MessageId
                  :sequence-number data.SequenceNumber})
             #(close! chan))))
    chan))
