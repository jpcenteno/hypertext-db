(ns hypertext-db.graph
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.graph.parser         :as parser]
            [hypertext-db.graph.backlinks-impl :as backlinks]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs : Graph attributes                                          ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- key-equals-node-id? [[k node]]
  (= k (::vault-file/id node)))

(s/def ::nodes (s/and (s/map-of ::vault-file/id ::node/t)
                      (s/every key-equals-node-id?)))

(s/def ::parser-chain ::parser/parser-chain)

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
                  (s/keys :req [::nodes ::backlinks ::parser-chain])
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
      (assoc ::parser-chain [])))

(s/fdef set-parsers
  :args (s/cat :graph ::t :parsers ::parser-chain)
  :ret  ::t)
(defn set-parsers
  [graph parsers]
  (assoc graph ::parser-chain parsers))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Node operations                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- pc-every-other-key-remains-unchanged
  "Post-condition which checks that a function can only alter certain keys.

  The function under test must take a `hash-map` as argument and and return an
  altered version of it. This post-condition asserts that every key other than
  those specified at `allow-listed-ks` remain unchanged.

  ## Parameters

  - `get-input-fn`: A `[[ifn?]]` that when applied to 
  "
  [get-input-fn allow-listed-ks]
  (fn [m]
    (let [dissoc-allow-listed-keys #(apply dissoc % allow-listed-ks)
          input-graph              (get-input-fn (:args m))
          output-graph             (:ret m)]
      (= (dissoc-allow-listed-keys input-graph)
         (dissoc-allow-listed-keys output-graph)))))

(defn- pc-does-not-contain-node
  [get-node-fn]
  (fn [{:keys [args ret]}]
    (not (contains-node? ret
                         (-> args get-node-fn node/id)))))

(defn- pc-every-other-node-remains-unchanged
  [get-graph-fn get-node-fn]
  (fn [m]
    (let [node-id-arg (-> m :args get-node-fn ::vault-file/id)
          nodes-arg   (-> m :args get-graph-fn ::nodes)
          nodes-ret   (-> m :ret ::nodes)]
      (= nodes-ret
         (dissoc nodes-arg node-id-arg)))))

(s/fdef disj-node*
  :args (s/or :1-ary (s/cat :graph ::t)
              :2-ary (s/and (s/cat :graph ::t :node ::node/t)
                            #(= (:node %)
                                (get-node (:graph %)
                                          (node/id (:node %))))))
  :ret  ::t
  :fn   (s/and
         (pc-every-other-key-remains-unchanged  #(-> % second :graph) [::nodes ::backlinks])
         (pc-every-other-node-remains-unchanged #(-> % second :graph) #(->> % second :node))
         (pc-does-not-contain-node              #(-> % second :node))))
(defn- disj-node*
  "Internal implementation of `disj-node`.
  
  Takes a `graph` and a node that must be contained in the graph as-is. Returns
  a new graph with those nodes removed.

  ## Caveats

  - `node` must be equal to the `::node/t` associated with the graph. Otherwise,
    it will fail to update the backlinks reciprocal graph correctly, thus
    returning an invalid `::graph/t`."
  ([graph] graph)
  ([graph node]
   (-> graph
       (update ::nodes dissoc (node/id node))
       (update ::backlinks backlinks/remove-from-node node))))

(s/fdef disj-node
  :args (s/cat :graph ::t :node ::node/t)
  :ret ::t
  #_(:fn (s/and (node-op-invariant-fn
                 (fn [ret graph node]
                   (let [nodes-arg (::nodes graph)
                         nodes-ret (::nodes ret)
                         node-id   (node/id node)]
                     (= nodes-ret (dissoc nodes-arg node-id))))))))
(defn disj-node
  "Disj[oin]. Returns a new `graph` that does not contain the `node`."
  [graph node]
  (if-let [node (get-in graph [::nodes (node/id node)])]
    (disj-node* graph node)
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
  :ret  ::t)
(defn add-node-from-vault-file
  [graph vault-file]
  (conj-node graph (parser/parse (::parser-chain graph) vault-file graph)))

;;;; Batch synchronization with the associated vault:

(defn- remove-nodes-that-no-longer-exist
  "Removes all the graph's nodes that no longer exist in its vault.

  Takes a `::graph/t` and the collection of all the `::vault-file/t`s that
  exist in the vault's storage."
  [graph vault-files]
  (let [nodes-in-graph  (-> graph ::nodes vals)
        ids-still-exist (set (map ::vault-file/id vault-files))]
    (transduce (filter (fn [node] (not (ids-still-exist (node/id node)))))
               disj-node*
               graph
               nodes-in-graph)))

(s/fdef batch-sync-graph-with-vault
  ;; Note that it's impossible to write a post-condition (`:fn`) for this
  ;; function due to it's non-deterministical and best-effort behavior.
  :args (s/cat :graph ::t)
  :ret  ::t)
(defn batch-sync-graph-with-vault
  "Batch syncs the graph with the contents of its associated vault.

  It takes a `::graph/t` as a parameter and reads its associated
  [[hypertext-db.vault]] backing storage to determine which nodes need to be
  updated or deleted. It returns an up-to-date version of the input
  `::graph/t` on a best-effort basis (see the caveats section below).

  ## Caveats

  This process is subject to running into a race condition, as the directory
  contents could change while the function is being executed. This function
  operates on a best-effort basis. In the **rare scenario** of a race
  condition, it will still return a good-enough approximation of the desired
  result."
  [graph]
  ;; Here we leverage the fact that `reduce` performs **eager** evaluation.
  (let [vault-files (vault/list-vault-files graph)]
    (reduce add-node-from-vault-file
            (remove-nodes-that-no-longer-exist graph vault-files)
            vault-files)))

(s/fdef upsert-node-given-full-path-
  :args (s/and (s/cat :graph ::t :absolute-file vault/absolute-file?)
               #(vault/absolute-file-in-vault? (:graph %) (:absolute-file %)))
  :ret ::t)
(defn upsert-node-given-full-path-
  "(Internal) upserts a node provided it's full path."
  [graph absolute-file]
  (if (.exists absolute-file)
    (add-node-from-vault-file
     graph
     (vault/absolute-file->vault-file graph absolute-file))
    graph))

(s/fdef remove-node-given-full-path-
  :args (s/cat :graph ::t :full-path #(instance? File %))
  :ret ::t)
(defn remove-node-given-full-path-
  "(Internal) removes a node with associated with the `full-path`"
  [graph full-path]
  (throw (UnsupportedOperationException. "Not implemented yet")))
