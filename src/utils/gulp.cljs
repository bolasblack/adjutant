(ns utils.gulp
  (:refer-clojure :exclude [-> merge]))

(try
  (def ^:private gulp (js/require "gulp"))
  (def ^:private merge-stream (js/require "merge-stream"))
  (catch js/Error e
    (println (.-stack e))))

(defn- list-to-js-obj [args]
  (clj->js (apply hash-map args)))

(defn -> [& streams]
  (reduce #(.pipe %1 %2) streams))

(defn src [& args]
  (let [[sources opts] (split-with string? args)]
    (gulp.src
     (clj->js sources)
     (list-to-js-obj opts))))

(defn dest [path & opts]
  (gulp.dest path (clj->js opts)))

(defn symlink [folder & opts]
  (gulp.symlink folder (list-to-js-obj opts)))

(defn task [& args]
  (apply gulp.task args))

(defn last-run [& args]
  (apply gulp.lastRun args))

(defn parallel [& args]
  (apply gulp.parallel args))

(defn series [& args]
  (apply gulp.series args))

(defn watch [& args]
  (let [fn (last args)
        [sources opts] (split-with string? (butlast args))]
    (gulp.watch
     (clj->js sources)
     (list-to-js-obj opts)
     fn)))

(defn tree [& opts]
  (js->clj
   (gulp.tree (list-to-js-obj opts))
   :keywordize-keys true))

(defn registry [registry]
  (gulp.registry registry))

(defn merge [& streams]
  (apply merge-stream streams))
