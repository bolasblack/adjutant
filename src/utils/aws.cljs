(ns utils.aws
  (:require [clojure.string :as str]
            ["aws-sdk" :as AWS]
            ["lodash.startcase" :as lodash-start-case]))

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

(defn tap-l [& msgs]
  (apply println msgs)
  (first msgs))

(defn tap-r [& msgs]
  (apply println msgs)
  (last msgs))

(defn paramify [params]
  (->> params
       (into {})
       (map (fn [[k v]] [(-> (name k)
                             lodash-start-case
                             (str/replace " " "")
                             keyword)
                         v]))
       (into {})))
