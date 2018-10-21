(ns utils.string
  (:require-macros
   [utils.core :refer [def-]]
   [utils.string :refer [<<]])
  (:require
   ["/js-vendors/lodash.startcase" :default lodash-start-case]
   ["/js-vendors/lodash.kebabcase" :default lodash-kebab-case]
   ["/js-vendors/lodash.lowerfirst" :default lodash-lower-first]
   ["/js-vendors/lodash.upperfirst" :default lodash-upper-first]
   [clojure.string :as str]))

(def- last-sym-re #"[\?\!]$")

(defn kebab-case
  "Converts string to [kebab case](https://en.wikipedia.org/wiki/Letter_case#Special_case_styles).
  Use `:keep-sym? true` to keep [Predicate Suffix `?`](https://clojure.org/guides/weird_characters#__code_symbol_code_predicate_suffix) and [Unsafe Operations `!`](https://clojure.org/guides/weird_characters#__code_symbol_code_unsafe_operations)

  (kebab-case \"Foo! Bar?\" :keep-sym? true)
  // => 'foo-bar?'

  (kebab-case \"foo?Bar!\" :keep-sym? true)
  // => 'foo-bar!'

  (kebab-case \"__FOO?_BAR__?\")
  // => 'foo-bar'"
  [s & {:keys [keep-sym?]}]
  {:pre [(or (string? s)
             (keyword? s))]}
  (let [transformed (lodash-kebab-case (name s))
        matched (re-find last-sym-re s)]
    (if (and matched keep-sym?)
      (str transformed matched)
      transformed)))

(defn start-case
  "Converts string to [start case](https://en.wikipedia.org/wiki/Letter_case#Title_case).
  Use `:keep-sym? true` to keep [Predicate Suffix `?`](https://clojure.org/guides/weird_characters#__code_symbol_code_predicate_suffix) and [Unsafe Operations `!`](https://clojure.org/guides/weird_characters#__code_symbol_code_unsafe_operations)

  (start-case \"--foo!-bar--!\" :keep-sym? true)
  // => 'Foo Bar!'

  (start-case \"foo?Bar?\" :keep-sym? true);
  // => 'Foo Bar?'

  (start-case \"__FOO?_BAR__!\")
  // => 'FOO BAR'"
  [s & {:keys [keep-sym?]}]
  {:pre [(or (string? s)
             (keyword? s))]}
  (let [transformed (lodash-start-case (name s))
        matched (re-find last-sym-re s)]
    (if (and matched keep-sym?)
      (str transformed matched)
      transformed)))

(defn sentence-case
  "Converts string to [sentence case](https://en.wikipedia.org/wiki/Letter_case#Sentence_case).
  Use `:keep-sym? true` to keep [Predicate Suffix `?`](https://clojure.org/guides/weird_characters#__code_symbol_code_predicate_suffix) and [Unsafe Operations `!`](https://clojure.org/guides/weird_characters#__code_symbol_code_unsafe_operations)

  (sentence-case \"--foo!-bar--!\" :keep-sym? true)
  // => 'Foo bar!'

  (sentence-case \"foo?Bar?\" :keep-sym? true);
  // => 'Foo bar?'

  (sentence-case \"__FOO?_BAR__!\")
  // => 'Foo bar'"
  [s & opts]
  (-> (apply start-case s opts)
      .toLowerCase
      lodash-upper-first))

(defn walk-keys [f hash]
  (letfn [(transform-pair [[k v]]
            [(f k) (if (map? v) (walk-keys f v) v)])]
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
