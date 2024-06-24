(ns zettel.graph
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [zettel.graph.node       :as node]
            [zettel.id               :as id]
            [zettel.graph.backlinks-impl :as backlinks]
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
(s/def ::backlinks ::backlinks/t)

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

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Observers                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef contains-node-with-id?
  :args (s/cat :graph ::t :id ::vault-file/id)
  :ret  boolean?)
(defn- contains-node-with-id?
  "Does the graph include a node with that `id`?"
  [graph id]
  (contains? (::nodes graph) id))

(s/fdef links-to?
  :args (s/cat :graph ::t :from ::vault-file/id :to ::vault-file/id)
  :ret  boolean?)
(defn links-to?
  "Does the node with id `from` have a directed link to the node with id `to`?"
  [graph from to]
  ;; Thanks to the spec for `::t` that specifies that backlinks are a reflection
  ;; of the links declared in the nodes from `::nodes`, we can use `::backlinks`
  ;; for a simpler implementation.
  (contains? (get-in graph [::backlinks to]) from))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Node operations                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef insert-node
  :args (s/cat :graph ::t :node ::node/t)
  :ret  ::t
  :fn   (s/and
         ;; NOTE that there is no need to add any predicate about `::backlinks`.
         ;; Those will be covered by the spec for `::t` provided we cover the
         ;; function invariants for `::nodes`.
         #(let [input-node    (get-in % [:args :node])
                input-node-id (::vault-file/id input-node)
                node-from-out (get-in % [:ret ::nodes input-node-id])]
            (= input-node node-from-out))
         ;; Every other node remains unchanged.
         #(let [input-node-id  (get-in % [:args :node ::vault-file/id])
                nodes-from-in  (get-in % [:args :graph ::nodes])
                nodes-from-out (get-in % [:ret ::nodes])]
            (= nodes-from-in (dissoc nodes-from-out input-node-id)))))
(defn insert-node
  [graph node]
  (let [id (::vault-file/id node)]
    (-> graph
        (assoc-in [::nodes id] node)
        (update ::backlinks backlinks/add-from-node node))))

(defn- node-op-invariant-fn [f]
  #(let [{:keys [ret args]} %
         {:keys [graph node]} args]
     (f ret graph node)))

;; FIXME this will break if we try to remove an updated node where
;; `::node/links` has changed.
(s/fdef remove-node
  :args (s/cat :graph ::t :node ::node/t)
  :ret ::t
  :fn (s/and (node-op-invariant-fn
              (fn [ret _graph node] (not (contains-node-with-id? ret node))))
             (node-op-invariant-fn
              (fn [ret graph node]
                (if (contains-node-with-id? graph (::vault-file/id node))
                  (= graph (insert-node ret node))
                  (= graph ret))))))
(defn remove-node
  [graph node]
  (-> graph
      (update ::nodes dissoc (::vault-file/id node))
      (update ::backlinks backlinks/remove-from-node node)))
