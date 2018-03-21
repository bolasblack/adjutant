(ns utils.string
  (:require ["lodash.startcase" :as lodash-start-case]
            ["lodash.kebabcase" :as lodash-kebab-case]
            [clojure.string :as str]))

(defn kebab-case [s & {:keys [keep-pred]}]
  {:pre [(string? s)]}
  (let [transformed (lodash-kebab-case s)
        matched (re-find #"[\?\!]$" s)]
    (if (and matched keep-pred)
      (str transformed matched)
      transformed)))

(defn start-case [s & {:keys [keep-pred]}]
  {:pre [(string? s)]}
  (let [transformed (lodash-start-case s)
        matched (re-find #"[\?\!]$" s)]
    (if (and matched keep-pred)
      (str transformed matched)
      transformed)))

(defn walk-keys [f hash]
  (letfn [(transform-pair [[k v]]
            [(f k)
             (if (map? v) (walk-keys f v) v)])]
    (into {} (map transform-pair hash))))

(defn kebab-case-keys [hash]
  (letfn [(transform-key [key]
            (cond (or (keyword? key)
                      (symbol? key))
                  (keyword (kebab-case (name key) :keep-pred true))

                  (string? key)
                  (kebab-case key :keep-pred true)

                  :else
                  key))]
    (walk-keys transform-key hash)))

(defn start-case-keys [hash]
  (letfn [(transform-key [key]
            (cond (or (keyword? key)
                      (symbol? key))
                  (keyword (start-case (name key) :keep-pred true))

                  (string? key)
                  (start-case key :keep-pred true)

                  :else
                  key))]
    (walk-keys transform-key hash)))
