(ns utils.async
  #?(:cljs (:require-macros [utils.async :refer [go go-loop go-let go-try-let <!]]))
  (:require
   #?(:cljs [goog.object :as go])
   [clojure.core.async.impl.protocols]
   [clojure.core.async
    :as async
    :refer [>! take! put! close!]]
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

#?(:clj (defmacro <! [ch]
          `(uc/if-cljs
            (cljs.core.async/<! ~ch)
            (clojure.core.async/<! ~ch))))



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
          (string? obj) (uc/error obj)
          :else (ex-info "" {:reason obj}))

    :node
    [obj nil]

    :cats-either
    (ce/left obj)

    (uc/error! (str "Unsupported policy: " policy))))

(defn unpack-value [obj & {:keys [policy]
                           :or {policy default-error-policy}}]
  (condp = policy
    :error
    obj

    :node
    (and obj
         (nth obj 1 nil))

    :cats-either
    (when (ce/right? obj)
      @obj)

    (uc/error! (str "Unsupported policy: " policy))))

(defn unpack-error [obj & {:keys [policy]
                           :or {policy default-error-policy}}]
  (condp = policy
    :error
    (let [data (ex-data obj)
          reason (:reason data)]
      (or reason obj))

    :node
    (and obj
         (nth obj 0 nil))

    :cats-either
    (when (ce/left? obj)
      @obj)

    (uc/error! (str "Unsupported policy: " policy))))


(defn throw-err [e & error?-opts]
  (if (apply error? e error?-opts)
    (if (uc/error? e)
      (throw e)
      (uc/error! "Wrap error from channel"
                 {::from-<? true :original e}))
    e))

#?(:clj (defmacro <? [ch & error?-opts]
          `(throw-err (<! ~ch) ~@error?-opts)))



#?(:cljs
   (defn promise->chan
     "Transform `js/Promise` to `cljs.core.async/chan`, wrap error
  with `(pack-error % :policy :error)` if not reject with `js/Error`"
     [promise]
     (let [chan (async/chan)]
       (.then
        promise
        #(put! chan %1 (fn [] (close! chan)))
        #(let [err (if (uc/error? %1)
                     %1
                     (pack-error %1 :policy :error))]
           (put! chan err (fn [] (close! chan)))))
       chan)))

#?(:cljs
   (defn chan->promise
     "Resolve `cljs.core.async/chan` next value to `js/Promise`,
  reject the unpacked result if `error?`"
     [chan & {:keys [policy]
              :or {policy default-error-policy}}]
     (js/Promise.
      (fn [resolve reject]
        (go-let [val (<! chan)]
          (if (error? val :policy policy)
            (reject (unpack-error val :policy policy))
            (resolve (unpack-value val :policy policy))))))))

#?(:clj (defmacro <p! [promise]
          `(<! (promise->chan ~promise))))

#?(:clj (defmacro <p? [promise & error?-opts]
          `(<? (promise->chan ~promise) ~@error?-opts)))



#?(:cljs
   (defn chan->async-iterator
     "Transform `cljs.core.async/chan` to AsyncIterator"
     [chan & {:keys [policy convert-to-js]
              :or {policy default-error-policy
                   convert-to-js false}}]
     (let [res #js {:next (fn [] (.then (chan->promise chan :policy policy)
                                       (fn [res]
                                         #js {:done (nil? res)
                                              :value (if convert-to-js
                                                       (if (some? res)
                                                         (clj->js res)
                                                         js/undefined)
                                                       res)})))}]
       (try (go/set res js/Symbol.asyncIterator (fn [] res)))
       res)))



(defn flat-chan [ch]
  (go-loop [c ch]
    (if (chan? c)
      (recur (<! c))
      c)))

#?(:clj (defmacro <<! [ch]
          `(<! (flat-chan ~ch))))

#?(:clj (defmacro <<? [ch & error?-opts]
          `(<? (flat-chan ~ch) ~@error?-opts)))



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
