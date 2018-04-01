(ns utils.string
  (:require ["lodash.startcase" :as lodash-start-case]
            ["lodash.kebabcase" :as lodash-kebab-case]
            ["lodash.lowerfirst" :as lodash-lower-first]
            ["lodash.upperfirst" :as lodash-upper-first]
            [clojure.string :as str]))

(defn kebab-case [s & {:keys [keep-pred?]}]
  {:pre [(or (string? s)
             (keyword? s))]}
  (let [transformed (lodash-kebab-case (name s))
        matched (re-find #"[\?\!]$" s)]
    (if (and matched keep-pred?)
      (str transformed matched)
      transformed)))

(defn start-case [s & {:keys [keep-pred?]}]
  {:pre [(or (string? s)
             (keyword? s))]}
  (let [transformed (lodash-start-case (name s))
        matched (re-find #"[\?\!]$" s)]
    (if (and matched keep-pred?)
      (str transformed matched)
      transformed)))

(defn sentence-case [s & opts]
  {:pre [(or (string? s)
             (keyword? s))]}
  (-> (apply start-case (name s) opts)
      .toLowerCase
      lodash-upper-first))

(defn walk-keys [f hash]
  (letfn [(transform-pair [[k v]]
            [(f k)
             (if (map? v) (walk-keys f v) v)])]
    (into {} (map transform-pair hash))))

(defn kebab-case-keys [hash & opts]
  (walk-keys
   (fn [key]
     (cond (or (keyword? key)
               (symbol? key))
           (keyword (apply kebab-case (name key) opts))

           (string? key)
           (apply kebab-case key opts)

           :else
           key))
   hash))

(defn start-case-keys [hash & opts]
  (walk-keys
   (fn [key]
     (cond (or (keyword? key)
               (symbol? key))
           (keyword (apply start-case (name key) opts))

           (string? key)
           (apply start-case key opts)

           :else
           key))
   hash))

(defn paramify [hash & {:keys [first-upper?]
                        :or {first-upper? false}}]
  (walk-keys
   (fn [key]
     (if (or (keyword? key)
             (symbol? key)
             (string? key))
       (-> (name key)
           lodash-start-case
           (str/replace " " "")
           ((if first-upper? identity lodash-lower-first))
           keyword)
       key))
   hash))
