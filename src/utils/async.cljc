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
                     (if (:from-<? ex-data#)
                       (:original ex-data#)
                       e#)
                     e#))))))

#?(:clj (defmacro go-try-let [binding & body]
          `(go-try (let ~binding ~@body))))

(defn error?
  "Error detect policy available values:

  * `:error`: (default) Expect `obj` is normal value, return true if `obj` is Error(js)/Exception(java)
  * `:node`: Expect `obj` is `coll?`, return true if `(some? (first obj))`
  * `:cats-either`: Expect `obj` is `cats.monad.either/Either`, return true if `(cats.monad.either/left? obj)`"
  [obj & {:keys [policy]
          :or {policy :error}}]
  (condp = policy
    :error (uc/error? obj)
    :node (some? (first obj))
    :cats-either (ce/left? obj)
    (uc/error! (str "Unsupported policy: " policy))))

(defn chan? [a]
  (satisfies? clojure.core.async.impl.protocols/ReadPort a))

(defn promise? [a]
  (and a (fn? (.-then a))))

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

#?(:cljs (defn from-promise [promise]
           (let [chan (async/chan)]
             (-> promise
                 (.then #(do (put! chan [nil %1])
                             (close! chan)))
                 (.catch #(do (put! chan [%1 nil])
                              (close! chan))))
             chan)))

(defn throw-err [e & error?-opts]
  (if (apply error? e error?-opts)
    (uc/error! "Error from channel"
               {:from-<? true :original e})
    e))

#?(:clj (defmacro <? [ch & error?-opts]
          `(uc/if-cljs
            (throw-err (cljs.core.async/<! ~ch) ~@error?-opts)
            (throw-err (clojure.core.async/<! ~ch) ~@error?-opts))))
