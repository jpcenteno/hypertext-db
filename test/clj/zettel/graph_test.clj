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

;; For most of the tests we will be using a simple graph with two nodes,
;; `node-a` and `node-b`, where `node-a` links to `node-b`, but `node-b`
;; does not link to `node-a`.
;;
;; [A] --> [B]

(s/fdef create-node
  :args (s/cat)
  :ret ::node/t)
(defn- create-node []
  (-> "some.ext" vault-file/random node/vault-file->))

(def ^:private node-b
  "A node without outgoing links."
  (create-node))

(def ^:private node-b-id (::vault-file/id node-b))

(def ^:private node-a
  "A node that links to `node-b`"
  (assoc (create-node) ::node/links #{node-b-id}))

(def ^:private node-a-without-links
  "A modified `node-a` without the link to `node-b`. Used for some of the
  tests."
  (assoc node-a ::node/links #{}))

(def ^:private node-a-id   (::vault-file/id node-a))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type spec tests and documentation                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

;; This test was originally writen to TDD the `::graph/t` type spec. It has been
;; since refactored to serve as documentation for the restrictions imposed by
;; the mentioned spec.
;;
;; This should be more relavant to mantainers rather than users of this library.
;;
;; As an implementation note, since we are dealing with examples that break the
;; `::graph/t` type invariants, we can't use any function that takes or returns
;; the mentioned data type. Doing so should raise an instrumentation error. On
;; one hand, this makes the code more tied to the implementation and harder to
;; follow, on the other hand, it showcases it's internal structure.
(deftest spec
  (tmp/with-tmp-dir
    (let [vault (vault/dir-> tmp/dir)
          graph (graph/vault-> vault)]

      (testing "In the `::graph/nodes` map"

        (testing "every key should match the node's id."
          ;; The map data structure was chosen for `::graph/nodes` to serve as an
          ;; index by node id, then, every key must match the value's node id.

          (testing "An offending graph"
            ;; In this example, the value for `node-a-id` has a different node id:
            (let [graph (assoc graph ::graph/nodes {node-a-id node-b})]

              (testing "is invalid"
                (is (not (s/valid? ::graph/t graph)))))

            (testing "can be fixed by replacing the value with the correct node"
              (let [graph (assoc-in graph [::graph/nodes node-a-id] node-a-without-links)]
                (is (s/valid? ::graph/t graph))))

            (testing "can be fixed by removing the offending element from `::graph/nodes`"
              (let [graph (update graph ::graph/nodes dissoc node-a-id)]
                (is (s/valid? ::graph/t graph))))))

        (testing "every link should have a reciprocal mapping in `::graph/backlinks`:"

          (testing "Invalid graph"
            (let [graph (-> graph
                            (assoc ::graph/nodes {node-a-id node-a}))]
              (is (not (s/valid? ::graph/t graph)))))
          (testing "Valid graph"
            (let [graph (-> graph
                            (assoc ::graph/nodes {node-a-id node-a})
                            (assoc ::graph/backlinks {node-b-id #{node-a-id}}))]
              (is (s/valid? ::graph/t graph))))))

      (testing "Links from ::graph/backlinks"

        (testing "must come from node id's contained by `::graph/nodes`:"

          (testing "Invalid graph"
            (let [graph (merge graph
                               {::graph/nodes     {}
                                ::graph/backlinks {node-b-id #{node-a-id}}})]
              (is (not (s/valid? ::graph/t graph)))))

          (testing "Valid graph"
            (let [graph (merge graph
                               {::graph/nodes     {node-a-id node-a}
                                ::graph/backlinks {node-b-id #{node-a-id}}})]
              (is (s/valid? ::graph/t graph)))))

        (testing "must be reciprocated by `::graph/notes`:"
            ;; Every edge described by `::graph/backlinks` is required to have a
            ;; reciprocal edge in the links described by `::graph/nodes`.
            ;;
            ;; Supose that we have the following `::graph/nodes` map:
            ;;
            ;; ```clojure
            ;; {node-a-id {::node/links #{node-b-id} ...}}
            ;; ```
            ;;
            ;; Then, `::graph/backlinks` must be equal to:
            ;;
            ;; ```clojure
            ;; {node-b-id #{node-a-id}}
            ;; ```

          (testing "An offending graph"
              ;; Here, we are using a modified version of `node-a` called
              ;; `node-a-without-links` where the link to `node-b-id` has been
              ;; removed.
              ;;
              ;; Here, our `::graph/nodes` map looks like:
              ;;
              ;; ```clojure
              ;; {node-a-id {::node/links #{}}
              ;;  node-b-id {::node/links #{}}}
              ;; ```
              ;; The graph described by `::graph/nodes` can be represented as:
              ;;
              ;; ```
              ;; [node-b-id]     [node-a-id]
              ;; ```
              ;;
              ;; At the same time we kept the same `::graph/backlinks` map from
              ;; the other examples:
              ;;
              ;; ```clojure
              ;; {node-b-id #{node-a-id}}
              ;; ```
              ;;
              ;; In this case, `::graph/backlinks` describes the following graph:
              ;;
              ;; ```
              ;; [node-b-id] <-- [node-a-id]
              ;; ```
            (let [graph (merge graph
                               {::graph/nodes     {node-a-id node-a-without-links
                                                   node-b-id   node-b}
                                ::graph/backlinks {node-b-id #{node-a-id}}})]
              (is (not (s/valid? ::graph/t graph)))

              (testing "can be fixed by adding the missing link to `::graph/nodes`"
                  ;; We can "fix" the previous graph by adding a link from
                  ;; `node-a-id` to `node-b-id`.
                  ;;
                  ;; Now, `::graph/nodes` describes the same graph as
                  ;; `::graph/backlinks`:
                  ;;
                  ;; ```
                  ;; [node-b-id] <-- [node-a-id]
                  ;; ```
                (let [graph (update-in graph [::graph/nodes node-a-id ::node/links] conj node-b-id)]
                  (is (s/valid? ::graph/t graph))))

              (testing "can be fixed by removing the offending backlink"
                  ;; Another way to fix the problem is to remove the offending
                  ;; backlink.
                  ;;
                  ;; Now, the `::graph/backlinks` map represents the following graph:
                  ;;
                  ;; ```clojure
                  ;; [node-b-id]
                  ;; ```
                  ;;
                  ;; Which is a subset from the graph represented by
                  ;; `::graph/nodes`.
                (let [graph (update-in graph [::graph/backlinks node-b-id] disj node-a-id)]
                  (is (s/valid? ::graph/t graph)))))))

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
                          (assoc ::graph/nodes     {node-a-id node-a})
                          (assoc ::graph/backlinks {node-b-id #{node-a-id}}))]
            (is (s/valid? ::graph/t graph))))))))

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
