(ns zettel.graph-test
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [clojure.test            :refer [deftest is testing]]
            [zettel.graph            :as graph]
            [zettel.graph.node       :as node]
            [zettel.helpers.tmp      :as tmp]
            [zettel.id               :as id]
            [zettel.vault            :as vault]
            [zettel.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Test data                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef create-node
  :args (s/cat)
  :ret ::node/t)
(defn- create-node []
  (-> "some.ext" vault-file/random node/vault-file->))

(def ^:private node-bob
  "A node without outgoing links."
  (create-node))

(def ^:private id-bob     (::vault-file/id node-bob))

(def ^:private node-alice
  "A node that links to `node-bob`"
  (assoc (create-node) ::node/links #{id-bob}))

(def ^:private node-alice-without-links
  "A modified `node-alice` without the link to `node-bob`."
  (assoc node-alice ::node/links #{}))

(def ^:private id-alice   (::vault-file/id node-alice))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type spec tests and documentation                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

(deftest spec
  (tmp/with-tmp-dir
    (let [vault (vault/dir-> tmp/dir)
          graph (graph/vault-> vault)]

      (testing "Every key in the `::graph/nodes` map must match the node's key:"
        (testing "Invalid graph"
          (let [graph (assoc graph ::graph/nodes {id-alice node-bob})]
            (is (not (s/valid? ::graph/t graph)))))
        (testing "Valid graph"
          (let [graph (assoc graph ::graph/nodes {id-bob node-bob})]
            (is (s/valid? ::graph/t graph)))))

      (testing "Every link from `::graph/nodes` have a corresponding mapping in `::graph/backlinks`:"
        (testing "Invalid graph"
          (let [graph (-> graph
                          (assoc ::graph/nodes {id-alice node-alice}))]
            (is (not (s/valid? ::graph/t graph)))))
        (testing "Valid graph"
          (let [graph (-> graph
                          (assoc ::graph/nodes {id-alice node-alice})
                          (assoc ::graph/backlinks {id-bob #{id-alice}}))]
            (is (s/valid? ::graph/t graph)))))

      (testing "Every backlink id from `::graph/backlinks` must be an existing key in `::graph/nodes`:"
        (testing "Invalid graph"
          (let [graph (merge graph
                             {::graph/nodes     {}
                              ::graph/backlinks {id-bob #{id-alice}}})]
            (is (not (s/valid? ::graph/t graph)))))

        (testing "Valid graph"
          (let [graph (merge graph
                             {::graph/nodes     {id-alice node-alice}
                              ::graph/backlinks {id-bob #{id-alice}}})]
            (is (s/valid? ::graph/t graph)))))

      (testing "Every backlink from `::graph/backlinks` must have a corresponding mapping in `::graph/notes`"

        (testing "A graph is invalid when a backlink is not reflected by ::graph/nodes"
          (let [graph (merge graph
                             {::graph/nodes     {id-alice node-alice-without-links}
                              ::graph/backlinks {id-bob #{id-alice}}})]
            (is (not (s/valid? ::graph/t graph)))))

        (testing "We can fix the graph by adding the link to the corresponding node"
          (let [graph (merge graph
                             {::graph/nodes     {id-alice node-alice}
                              ::graph/backlinks {id-bob #{id-alice}}})]
            (is (s/valid? ::graph/t graph)))))

      (testing "A graph may contain backlinks to unknown nodes"
        ;; By design, nodes contain link ids without any knowledge about which
        ;; other nodes belong to the graph (The node domain is a layer below the
        ;; graph domain).
        ;;
        ;; A node might link to a node that wasn't added yet to the graph
        ;; or a broken link in the worst case scenario.
        ;;
        ;; Every operation that adds a node to the graph must replicate every
        ;; backlink in hopes that the linked node will be added in the future.
        (let [graph (-> graph
                        (assoc ::graph/nodes {id-alice node-alice})
                        (assoc ::graph/backlinks {id-bob #{id-alice}}))]
          (is (s/valid? ::graph/t graph)))))))

(deftest vault->
  (tmp/with-tmp-dir
    (testing "Returns a graph"
      (let [some-vault (vault/dir-> tmp/dir)
            graph      (graph/vault-> some-vault)]
        (is (s/valid? ::graph/t graph))

        (testing "that retails every attribute from the input vault"
          (is (set/subset? (set some-vault)
                           (set graph))))

        (testing "without any notes"
          (is (empty? (::notes graph))))

        (testing "without any backlinks"
          (is (empty? (::backlinks graph))))))))

(comment
  (deftest graph-type-spec
    (testing "Each key in ::nodes matches the node's id"
      (tmp/with-tmp-dir
        (let [node              (create-node)
              mismatching-id    (id/random)
              graph             (-> tmp/dir
                                    vault/dir->
                                    graph/vault->)]
          (is (s/valid? ::graph/t graph))
          (is (not (s/valid? ::graph/t (assoc-in graph  [::nodes mismatching-id] node)))))))

    (testing "Each node backlink should point to an existing node"
    ;; Otherwise, where did the backlink came from?
      (tmp/with-tmp-dir
        (let [bad-id (id/random)
              node   (update (create-node) ::backlinks conj bad-id)
              graph  (-> tmp/dir vault/dir-> graph/vault-> (graph/insert-node node))]
          (is (not (s/valid? ::graph/t graph))))))))
