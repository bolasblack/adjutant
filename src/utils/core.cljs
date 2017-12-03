(ns utils.core)

(let [result (atom nil)
      executed (atom false)]
  (defn once [f]
    (fn [args]
      (when-not (deref executed)
        (swap! result (fn [] (apply f args)))
        (swap! executed (fn [] true)))
      (deref result))))
