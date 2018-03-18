(ns utils.async
  #?(:cljs (:require-macros [utils.async :refer [go go-loop go-let go-try-let]]))
  (:require [clojure.core.async.impl.protocols]
            [clojure.core.async
             :as async
             :refer [<! >! take! put! close!]]
            [cats.monad.either :as ce]
            [utils.core :as uc]))

#?(:clj (defmacro go [& body]
          `(uc/if-cljs
            (cljs.core.async.macros/go ~@body)
            (clojure.core.async/go ~@body))))

#?(:clj (defmacro go-loop [binding & body]
          `(go (loop ~binding ~@body))))

#?(:clj (defmacro go-let [binding & body]
          `(go (let ~binding ~@body))))

#?(:clj (defmacro go-try [& body]
          `(go (try
                 ~@body
                 (catch (uc/if-cljs js/Error Throwable) e#
                   (if-let [ex-data# (ex-data e#)]
                     (if (::from-<? ex-data#)
                       (:original ex-data#)
                       e#)
                     e#))))))

#?(:clj (defmacro go-try-let [binding & body]
          `(go-try (let ~binding ~@body))))

(defn chan? [a]
  (satisfies? clojure.core.async.impl.protocols/ReadPort a))

(defn promise? [obj]
  (and obj (fn? (.-then obj))))

(def default-error-policy :error)

(defn error?
  "Detect if `obj` is error, available policy:

  * `:error`: (default) Expect `obj` is normal value, return true if `obj` is Error(js)/Exception(java)
  * `:node`: Expect `obj` is `coll?`, return true if `(some? (first obj))`
  * `:cats-either`: Expect `obj` is `cats.monad.either/Either`, return true if `(cats.monad.either/left? obj)`"
  [obj & {:keys [policy]
          :or {policy default-error-policy}}]
  (condp = policy
    :error (uc/error? obj)
    :node (some? (first obj))
    :cats-either (ce/left? obj)
    (uc/error! (str "Unsupported policy: " policy))))

(defn pack-value [obj & {:keys [policy]
                         :or {policy default-error-policy}}]
  (condp = policy
    :error
    obj

    :node
    [nil obj]

    :cats-either
    (ce/right obj)

    (uc/error! (str "Unsupported policy: " policy))))

(defn pack-error [obj & {:keys [policy]
                         :or {policy default-error-policy}}]
  (condp = policy
    :error
    (cond (uc/error? obj) obj
          (string? obj) (js/Error. obj)
          :else (ex-info "" {:reason obj}))

    :node
    [obj nil]

    :cats-either
    (ce/left obj)

    (uc/error! (str "Unsupported policy: " policy))))

(defn limit-map [f source limit-chan]
  (let [src-chan (if (chan? source)
                   source
                   (async/to-chan source))
        dst-chan (async/chan)]
    (go-loop []
      (<! limit-chan)
      (if-let [d (<! src-chan)]
        (let [r (f d)]
          (if (chan? r)
            (take! r #(put! dst-chan %))
            (put! dst-chan r))
          (recur))
        (close! dst-chan)))
    dst-chan))

#?(:cljs
   (defn from-promise
     "Transform `js/Promise` to `cljs.core.async/chan`, wrap
  value with `pack-value`, `pack-error`"
     [promise & {:keys [policy]
                 :or {policy default-error-policy}}]
     (let [chan (async/chan)]
       (.then
        promise
        #(do (put! chan (apply pack-value [%1 :policy policy]))
             (close! chan))
        #(do (put! chan (apply pack-error [%1 :policy policy]))
             (close! chan)))
       chan)))

(defn throw-err [e & error?-opts]
  (if (apply error? e error?-opts)
    (uc/error! "Error from channel"
               {::from-<? true :original e})
    e))

#?(:clj (defmacro <? [ch & error?-opts]
          `(uc/if-cljs
            (throw-err (cljs.core.async/<! ~ch) ~@error?-opts)
            (throw-err (clojure.core.async/<! ~ch) ~@error?-opts))))
