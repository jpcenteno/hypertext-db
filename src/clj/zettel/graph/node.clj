(ns zettel.graph.node
  "This namespace defines a model for graph nodes."
  (:require [clojure.spec.alpha      :as s]
            [zettel.id               :as id]
            [zettel.graph.link       :as link]
            [zettel.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def ^:private id-set (s/coll-of ::id/t :kind set?))
(s/def ::links     id-set)
(s/def ::backlinks id-set)

(s/def ::t (s/and
            (s/keys :req [::links ::backlinks])
            ::vault-file/t
                  ;; Self-referential links are disallowed for simplicity sake.
            #(not (contains? (::links %) (::vault-file/id %)))
            #(not (contains? (::backlinks %) (::vault-file/id %)))))
; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Constructor                                                            ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef vault-file->
  :args (s/cat :vault-file ::vault-file/t)
  :ret  ::t)
(defn vault-file->
  "Casts `vault-file` into a graph node without any links or backlinks."
  [vault-file]
  (-> vault-file
      (assoc ::links     #{})
      (assoc ::backlinks #{})))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Observers                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef ->links
  :args (s/cat :node ::t)
  :ret  (s/coll-of ::link/t)
  :fn   (s/and #(= (-> % :ret count) (-> % :args :node ::links count))
               #(every? (fn [[from _]] (= from (-> % :args :node ::vault-file/id))) (:ret %))
               #(every? (fn [[_ to]] (contains? (-> % :args :node ::links) to)) (:ret %))))
(defn ->links
  [node]
  (map (fn [to] [(::vault-file/id node) to])
       (::links node)))
