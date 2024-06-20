(ns zettel.graph.node
  (:require [clojure.spec.alpha      :as s]
            [zettel.id               :as id]
            [zettel.vault.vault-file :as vault-file]))

(def ^:private id-set (s/coll-of ::id/t :kind set?))
(s/def ::links     id-set)
(s/def ::backlinks id-set)

(s/def ::t (s/and ::vault-file/t
                  (s/keys :req [::links ::backlinks])
                  ;; Self-referential links are disallowed for simplicity sake.
                  #(not (contains? (::links %) (::vault-file/id %)))
                  #(not (contains? (::backlinks %) (::vault-file/id %)))))

(s/fdef vault-file->
  :args (s/cat :vault-file ::vault-file/t)
  :ret  ::t)
(defn vault-file->
  "Casts `vault-file` into a graph node without any links or backlinks."
  [vault-file]
  (-> vault-file
      (assoc ::links     #{})
      (assoc ::backlinks #{})))
