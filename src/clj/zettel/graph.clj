(ns zettel.graph
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [zettel.graph.node       :as node]
            [zettel.id               :as id]
            [zettel.vault            :as vault]
            [zettel.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs : Graph                                                     ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- key-equals-node-id? [[k node]]
  (= k (::vault-file/id node)))

(defn- set-does-not-contain-key? [[k ids]]
  (not (contains? ids k)))

(s/def ::nodes (s/and (s/map-of ::id/t ::node/t)
                      (s/every key-equals-node-id?)))

;; Backlinks exists to keep track of links to nodes that are not in the graph
;; yet or do not exist.
(s/def ::backlinks (s/and (s/map-of ::id/t ::node/backlinks)))

(s/fdef has-backlink?
  :args (s/cat :backlinks ::backlinks :from ::id/t :to ::id/t)
  :ret  boolean?)
(defn- has-backlink? [backlinks from to]
  (contains? (get backlinks to #{}) from))

(s/fdef every-link-in-backlinks?
  :args (s/cat :k (s/keys :req [::nodes ::backlinks]))
  :ret  boolean?)
(defn- every-link-in-backlinks? [{::keys [nodes backlinks]}]
  (let [links (->> nodes
                   (#(map    (fn [[from node]] [from (::node/links node)]) %))
                   (#(mapcat (fn [[from links]] (map (fn [to] [from to]) links)) %)))]
    (every? (fn [[from to]] (has-backlink? backlinks from to))
            links)))

(defn- flattened-backlinks
  [backlinks]
  (mapcat (fn [[to froms]] (map (fn [from] {:to to :from from})
                                froms))
          backlinks))
(defn- has-link?
  [nodes {:keys [from to]}]
  (contains? (-> nodes (get from) ::node/links) to))
(defn- every-backlink-in-links?
  [{::keys [nodes backlinks]}]
  (let [bs (flattened-backlinks backlinks)]
    (every? #(has-link? nodes %) bs)))

(comment
  (->
   {::nodes {1 {::node/links #{}}
             2 {::node/links #{1}}
             3 {::node/links #{1 2}}}
    ::backlinks {1 #{2 3}
                 2 #{3}}}
   every-backlink-in-links?))

(s/def ::t (s/and ::vault/t
                  (s/keys :req [::nodes ::backlinks])
                  every-link-in-backlinks?
                  every-backlink-in-links?))

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
