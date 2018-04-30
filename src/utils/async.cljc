(ns utils.async
  (:refer-clojure :exclude [into])
  #?(:cljs (:require-macros
            [cljs.core.async]
            [utils.async :refer [go go-loop go-let <! <p!]]))
  (:require
   #?(:cljs [goog.object :as go])
   #?(:cljs ["util" :refer [promisify]])
   [clojure.string :as s]
   [clojure.core.async.impl.protocols]
   [clojure.core.async
    :as async
    :refer [>! take! put! close!]]
   [utils.core :as uc :include-macros true]))



(defn- args-hashify [opts]
  (if (and (= 1 (count opts))
           (map? (first opts)))
    (first opts)
    (apply hash-map opts)))



#?(:clj (defmacro go [& body]
          `(uc/if-cljs
            (cljs.core.async/go ~@body)
            (clojure.core.async/go ~@body))))

#?(:clj (defmacro go-loop [binding & body]
          `(uc/if-cljs
            (cljs.core.async/go-loop ~binding ~@body)
            (clojure.core.async/go-loop ~binding ~@body))))

#?(:clj (defmacro <! [ch]
          `(uc/if-cljs
            (cljs.core.async/<! ~ch)
            (clojure.core.async/<! ~ch))))

#?(:clj (defmacro go-let [binding & body]
          `(go (let ~binding ~@body))))

#?(:clj (defmacro go-try [& body]
          `(go (try
                 ~@body
                 (catch (uc/if-cljs js/Error Throwable) e#
                   (unpack-error e#))))))

#?(:clj (defmacro go-try-let [binding & body]
          `(go-try (let ~binding ~@body))))

#?(:clj (defmacro go-try-loop [binding & body]
          `(go-try (loop ~binding ~@body))))



(defn chan? [a]
  (satisfies? clojure.core.async.impl.protocols/ReadPort a))

#?(:cljs
   (defn promise? [obj]
     (and obj (fn? (.-then obj)))))



(defn packed-error? [o]
  (-> (ex-data o)
      ::packed-error?))

(defn pack-error [o]
  (if (packed-error? o)
    o
    (ex-info (str "Packed error: "
                  (if (some? o)
                    (or (.-message o) (.toString o))
                    ""))
             {::packed-error? true :reason o})))

(defn unpack-error [o]
  (if (packed-error? o)
    (-> (ex-data o)
        :reason)
    o))

(defn packed-value? [o]
  (::packed-value? o))

(defn pack-value [o]
  (if (packed-value? o)
    o
    {::packed-value? true
     :value o}))

(defn unpack-value [o]
  (if (packed-value? o)
    (:value o)
    o))



(defn throw-err [e & opts]
  (let [final-opts (args-hashify opts)
        failure? (or (:failure? final-opts)
                     uc/error?)]
    (if (failure? e)
      (throw (pack-error e))
      e)))

#?(:clj (defmacro <? [ch & opts]
          `(throw-err (<! ~ch) ~@opts)))



#?(:cljs
   (defn chan->promise
     "Resolve `cljs.core.async/chan` next value to `js/Promise`"
     [chan & opts]
     (let [{:keys [failure? transform]
            :or {failure? uc/error?
                 transform identity}}
           (args-hashify opts)]
       (js/Promise.
        (fn [resolve reject]
          (go-let [_val (<! chan)
                   val (transform _val)]
            (if (failure? _val)
              (reject val)
              (resolve val))))))))

#?(:cljs
   (defn promise->chan
     "Transform `js/Promise` to `cljs.core.async/chan`"
     [promise & {:keys [pack-value pack-error]
                 :or {pack-value pack-value
                      pack-error pack-error}}]
     (let [chan (async/chan)]
       (.then
        promise
        #(put! chan (pack-value %1) (fn [] (close! chan)))
        #(put! chan (pack-error %1) (fn [] (close! chan))))
       chan)))

#?(:clj (defmacro <p! [promise & opts]
          `(<! (promise->chan ~promise ~@opts))))

#?(:clj (defmacro <p? [promise & opts]
          `(<? (promise->chan ~promise ~@opts) ~@opts)))



#?(:cljs
   (defn chan->async-iterator
     "Transform `cljs.core.async/chan` to AsyncIterator, pass
  all options to `chan->promise`, default `:transform` become
  `clj->js`"
     [chan & opts]
     (let [opts
           (merge {:transform clj->js}
                  (args-hashify opts))

           res
           #js {:next (fn [] (.then (chan->promise chan opts)
                                   (fn [res]
                                     #js {:done (nil? res)
                                          :value res})))}]
       (try (go/set res js/Symbol.asyncIterator (fn [] res)))
       res)))



(defn flat-chan [ch]
  (go-loop [c ch]
    (if (chan? c)
      (recur (<! c))
      c)))

#?(:clj (defmacro <<! [ch]
          `(<! (flat-chan ~ch))))

#?(:clj (defmacro <<? [ch & opts]
          `(<? (flat-chan ~ch) ~@opts)))



(defn limit-map [f source limit-chan]
  (let [src-chan (if (chan? source)
                   source
                   (async/to-chan source))
        dst-chan (async/chan)
        wait-all-put-chan (volatile! (async/to-chan [:start]))]
    (go-loop []
      (<! limit-chan)
      (if-let [data (<! src-chan)]
        (let [r (f data)]
          (if (chan? r)
            (vswap! wait-all-put-chan
                    (fn [old-chan]
                      (go-let [resp (<! r)]
                        (<! old-chan)
                        (>! dst-chan resp))))
            (vswap! wait-all-put-chan
                    (fn [old-chan]
                      (go (<! old-chan)
                          (>! dst-chan r)))))
          (recur))
        (do (<! @wait-all-put-chan)
            (close! dst-chan))))
    dst-chan))



#?(:cljs
   (defn denodify
     "Returns a function that will wrap the given `nodeFunction`.
  Instead of taking a callback, the returned function will return
  a `cljs.core.async/chan` whose fate is decided by the callback
  behavior of the given node function. The node function should
  conform to node.js convention of accepting a callback as last
  argument and calling that callback with error as the first
  argument and success value on the second argument.

  If the `nodeFunction` calls its callback with multiple success
  values, the fulfillment value will be an array of them.

  If you pass a `receiver`, the `nodeFunction` will be called as a
  method on the `receiver`.

  Example of promisifying the asynchronous `readFile` of node.js `fs`-module:

  ```clojurescript
  (def read-file (denodify (.-readFile fs)))

  (go (try
        (let [content (<? (read-file \"myfile\" \"utf8\") :failure? first)]
          (println \"The result of evaluating myfile.js\" (.toString content)))
        (catch js/Error err
          (prn 'Error reading file' err))))
  ```

  Note that if the node function is a method of some object, you
  can pass the object as the second argument like so:

  ```clojurescript
  (def redis-get (denodify (.-get redisClient) redisClient))

  (go (<! (redis-get \"foo\")))
  ```
  "
     ([f]
      (denodify f nil))
     ([f receiver]
      (let [promisify-fn (promisify f)
            denodified-fn (fn denodified-fn [& args]
                            (go (<p! (.apply promisify-fn receiver (apply array args)))))]
        (try
          (js/Object.defineProperty
           denodified-fn
           "length"
           #js {:configurable true :value (if (zero? (.-length promisify-fn))
                                            0
                                            (dec (.-length promisify-fn)))})
          (let [new-name (if (s/blank? (.-name f))
                           "denodified_fn"
                           (str "denodified_" (.-name f)))]
            (js/Object.defineProperty
             denodified-fn
             "name"
             #js {:configurable true :value new-name}))
          (catch js/Error err
            (js/console.error err)))
        denodified-fn))))

#?(:clj
   (defmacro denodify..
     "```clojurescript
  (go (<! (denodify.. fs.readFile \"foo\")))
  (macroexpand-1 '(denodify.. fs.readFile \"foo\"))
  #=> ((denodify fs.readFile) \"foo\")

  (go (<! (denodify.. redisClient -get \"foo\")))
  (macroexpand-1 '(denodify.. redisClient -get \"foo\"))
  #=> ((denodify (.. redisClient -get) redisClient) \"foo\")
  ```"
     ([o & _path]
      (let [[path args] (split-with #(and (symbol? %)
                                          (s/starts-with? (name %) "-"))
                                    _path)]
        (cond
          (empty? path)
          `((denodify ~o) ~@args)

          (= 1 (count path))
          `((denodify (.. ~o ~@path) ~o) ~@args)

          :else
          `((denodify (.. ~o ~@path) (.. ~o ~@(butlast path))) ~@args))))))



(defn into
  ([coll ch]
   (into coll ch :reject-onfailed? false))
  ([coll ch & opts]
   (let [{:keys [failure? reject-onfailed?]
          :or {failure? uc/error?
               reject-onfailed? true}}
         (args-hashify opts)]
     (if-not reject-onfailed?
       (async/into coll ch)
       (async/reduce
        (fn [result o]
          (if (failure? o)
            (reduced o)
            (conj result o)))
        coll
        ch)))))
