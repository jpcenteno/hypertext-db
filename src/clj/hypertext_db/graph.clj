(ns hypertext-db.graph
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.graph.backlinks-impl :as backlinks]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs : Graph attributes                                          ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- key-equals-node-id? [[k node]]
  (= k (::vault-file/id node)))

(s/def ::nodes (s/and (s/map-of ::vault-file/id ::node/t)
                      (s/every key-equals-node-id?)))

(s/def ::parsers (s/coll-of fn? :kind sequential?))

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
                  (s/keys :req [::nodes ::backlinks ::parsers])
                  every-link-has-its-mirroring-backlink?
                  every-backlink-has-its-mirroring-link?))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Observers                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef node-count
  :args (s/cat :graph ::t)
  :ret (s/and integer? (partial <= 0)))
(defn node-count
  "Returns the number of nodes in the graph"
  [graph]
  (-> graph ::nodes count))

(s/fdef contains-node?
  :args (s/cat :graph ::t :node-id ::vault-file/id)
  :ret boolean?)
(defn contains-node?
  [graph node-id]
  (contains? (::nodes graph) node-id))

(s/fdef get-node
  :args (s/cat :graph ::t :id ::vault-file/id)
  :ret  (s/nilable ::node/t))
(defn get-node
  [graph node-id]
  (get-in graph [::nodes node-id]))

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
      (assoc ::backlinks {})
      (assoc ::parsers [])))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Node operations                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- node-op-invariant-fn [f]
  #(let [{:keys [ret args]} %
         {:keys [graph node]} args]
     (f ret graph node)))

(s/fdef disj-node
  :args (s/cat :graph ::t :node ::node/t)
  :ret ::t
  :fn (s/and (node-op-invariant-fn
              (fn [ret graph node]
                (let [nodes-arg (::nodes graph)
                      nodes-ret (::nodes ret)
                      node-id   (node/id node)]
                  (= nodes-ret (dissoc nodes-arg node-id)))))))
(defn disj-node
  "Disj[oin]. Returns a new `graph` that does not contain the `node`."
  [graph node]
  (if-let [node (get-in graph [::nodes (node/id node)])]
    (-> graph
        (update ::nodes dissoc (::vault-file/id node))
        (update ::backlinks backlinks/remove-from-node node))
    graph))

(s/fdef conj-node
  :args (s/cat :graph ::t :node ::node/t)
  :ret  ::t
  :fn   (s/and #(let [node (-> % :args :node)]
                  (= node (get-in % [:ret ::nodes (node/id node)])))
               #(let [id  (get-in % [:args :node ::vault-file/id])]
                  (= (dissoc (get-in % [:args :graph ::nodes]) id)
                     (dissoc (get-in % [:ret ::nodes])         id)))))
(defn conj-node
  "Conj[oin] node. Returns a new `graph` with the `node` 'added'."
  [graph node]
  (let [id (::vault-file/id node)]
    (-> graph
        (disj-node node)
        (assoc-in [::nodes id] node)
        (update ::backlinks backlinks/add-from-node node))))

(s/fdef add-node-from-vault-file
  :args (s/cat :graph ::t :vault-file ::vault-file/t)
  :ret  ::t
  :fn   #(let [arg-vault-file (-> % :args :vault-file)
               ret-node       (-> % :ret (get-node (::vault-file/id arg-vault-file)))]
           (= (node/vault-file-> arg-vault-file) ret-node)))
(defn add-node-from-vault-file
  [graph vault-file]
  (conj-node graph (node/vault-file-> vault-file)))
