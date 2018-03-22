(ns utils.aws
  (:require ["aws-sdk" :as AWS]))

(let [aws-profile js/process.env.AWS_PROFILE
      aws-key js/process.env.AWS_KEY
      aws-secret js/process.env.AWS_SECRET]
  (cond
    aws-profile
    (set! (.. AWS -config -credentials)
          (AWS/SharedIniFileCredentials. #js {:profile aws-profile}))

    (and aws-key aws-secret)
    (set! (.-config AWS)
          (AWS/Config. {:accessKeyId aws-key
                        :secretAccessKey aws-secret}))))
