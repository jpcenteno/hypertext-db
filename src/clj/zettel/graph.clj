(ns zettel.graph
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [zettel.graph.node       :as node]
            [zettel.id               :as id]
            [zettel.vault            :as vault]
            [zettel.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs : Graph attributes                                          ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- key-equals-node-id? [[k node]]
  (= k (::vault-file/id node)))

(s/def ::nodes (s/and (s/map-of ::id/t ::node/t)
                      (s/every key-equals-node-id?)))

;; Backlinks exists to keep track of links to nodes that are not in the graph
;; yet or do not exist.
(s/def ::backlinks (s/and (s/map-of ::id/t ::node/backlinks)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private ::graph/t spec helpers                                         ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- has-link?
  [nodes [from to]]
  (contains? (get-in nodes [from ::node/links]) to))

(defn- flatten-backlinks-set
  [[to from-ids]]
  (map (fn [from] [from to]) from-ids))

(defn- flatten-backlinks-map
  [backlinks]
  (mapcat flatten-backlinks-set backlinks))

(defn- every-backlink-has-its-mirroring-link?
  [{::keys [nodes backlinks]}]
  (every? (partial has-link? nodes)
          (flatten-backlinks-map backlinks)))

(defn- has-backlink? [backlinks [from to]]
  (contains? (get backlinks to) from))

(defn- flatten-links-from-node
  [node]
  (let [from   (::vault-file/id node)
        to-ids (::node/links node)]
    (map (fn [to] [from to]) to-ids)))

(defn- flatten-links-from-nodes-map
  [nodes]
  (mapcat flatten-links-from-node nodes))

(defn- every-link-has-its-mirroring-backlink?
  [{::keys [nodes backlinks]}]
  (every? (partial has-backlink? backlinks)
          (flatten-links-from-nodes-map (vals nodes))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type spec: ::graph/t                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::t (s/and ::vault/t
                  (s/keys :req [::nodes ::backlinks])
                  every-link-has-its-mirroring-backlink?
                  every-backlink-has-its-mirroring-link?))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Constructor                                                            ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef vault->
  :args (s/cat :vault ::vault/t)
  :ret  ::t
  :fn   (s/and #(set/subset? (-> % :args :vault set) (-> % :ret set))
               #(-> % :ret ::nodes empty?)))
(defn vault->
  "Casts a `::vault/t` into an empty map"
  [vault]
  (-> vault
      (assoc ::nodes {})
      (assoc ::backlinks {})))

(s/fdef insert-node
  :args (s/cat :graph ::t :node ::node)
  :ret  ::t)
(defn insert-node
  [graph node]
  (update graph ::nodes assoc node))
