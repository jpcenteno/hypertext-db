(ns hypertext-db.specs.fn)

(defn returns-argument
  [get-argument-fn]
  (fn [{:keys [args ret]}]
    (= ret (get-argument-fn args))))

(defn allowed-keys
  [get-argument-fn ks]
  (fn [{:keys [args ret]}]
    (= (apply dissoc (get-argument-fn args) ks)
       (apply dissoc ret ks))))
