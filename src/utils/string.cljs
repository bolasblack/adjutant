(ns utils.string
  (:require ["lodash.startcase" :as lodash-start-case]
            ["lodash.kebabcase" :as lodash-kebab-case]
            [clojure.string :as str]))

(defn kebab-case [s]
  (let [transformed (lodash-kebab-case s)
        matched (re-find #"[^\w]$" s)]
    (if matched
      (str transformed matched)
      transformed)))

(defn- kebab-case-key [key]
  (cond
    (keyword? key) (-> (name key)
                       kebab-case
                       keyword)
    (string? key) (kebab-case key)
    true key))

(defn kebab-case-keys [hash]
  (->> (map #(vector (kebab-case-key (first %))
                     (let [val (last %)]
                       (if (map? val)
                         (kebab-case-keys val)
                         val))) hash)
       (into {})))

(defn start-case
  ([s]
   (start-case {} s))
  ([opts s]
   (let [to-str (fn [s]
                  (if (or (keyword? s)
                          (symbol? s))
                    (name s)
                    s))
         opt-contains-str (fn [key s]
                            (and opts
                                 (key opts)
                                 (->> (key opts)
                                      (map to-str)
                                      (map str/lower-case)
                                      set
                                      (#(contains? % (str/lower-case (to-str s)))))))
         s (to-str s)
         full-upper? (opt-contains-str :full-upper s)
         full-lower? (opt-contains-str :full-lower s)
         transformed (cond-> s
                       full-upper? str/upper-case
                       full-lower? str/lower-case
                       (every? #(not (true? %)) [full-upper? full-lower?]) lodash-start-case)
         matched (re-find #"[^\w]$" s)]
     (if matched
       (str transformed matched)
       transformed))))
