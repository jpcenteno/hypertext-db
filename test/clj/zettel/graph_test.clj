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

;; This test was originally writen to TDD the `::graph/t` type spec. It has been
;; since refactored to serve as documentation for the restrictions imposed by
;; the mentioned spec.
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

      (testing "Links from ::graph/backlinks"

        (testing "must come from node id's contained by `::graph/nodes`:"

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

        (testing "must be reciprocated by `::graph/notes`:"
          ;; Every edge described by `::graph/backlinks` is required to have a
          ;; reciprocal edge in the links described by `::graph/nodes`.
          ;;
          ;; Supose that we have the following `::graph/nodes` map:
          ;;
          ;; ```clojure
          ;; {id-alice {::node/links #{id-bob} ...}}
          ;; ```
          ;;
          ;; Then, `::graph/backlinks` must be equal to:
          ;;
          ;; ```clojure
          ;; {id-bob #{id-alice}}
          ;; ```

          (testing "An offending graph"
            ;; Here, we are using a modified version of `node-alice` called
            ;; `node-alice-without-links` where the link to `id-bob` has been
            ;; removed.
            ;;
            ;; Here, our `::graph/nodes` map looks like:
            ;;
            ;; ```clojure
            ;; {id-alice {::node/links #{}}
            ;;  id-bob {::node/links #{}}}
            ;; ```
            ;; The graph described by `::graph/nodes` can be represented as:
            ;;
            ;; ```
            ;; [id-bob]     [id-alice]
            ;; ```
            ;;
            ;; At the same time we kept the same `::graph/backlinks` map from
            ;; the other examples:
            ;;
            ;; ```clojure
            ;; {id-bob #{id-alice}}
            ;; ```
            ;;
            ;; In this case, `::graph/backlinks` describes the following graph:
            ;;
            ;; ```
            ;; [id-bob] <-- [id-alice]
            ;; ```
            (let [graph (merge graph
                               {::graph/nodes     {id-alice node-alice-without-links
                                                   id-bob   node-bob}
                                ::graph/backlinks {id-bob #{id-alice}}})]
              (is (not (s/valid? ::graph/t graph)))

              (testing "can be fixed by adding the missing link to `::graph/nodes`"
                ;; We can "fix" the previous graph by adding a link from
                ;; `id-alice` to `id-bob`.
                ;;
                ;; Now, `::graph/nodes` describes the same graph as
                ;; `::graph/backlinks`:
                ;;
                ;; ```
                ;; [id-bob] <-- [id-alice]
                ;; ```
                (let [graph (update-in graph [::graph/nodes id-alice ::node/links] conj id-bob)]
                  (is (s/valid? ::graph/t graph))))

              (testing "can be fixed by removing the offending backlink"
                ;; Another way to fix the problem is to remove the offending
                ;; backlink.
                ;;
                ;; Now, the `::graph/backlinks` map represents the following graph:
                ;;
                ;; ```clojure
                ;; [id-bob]
                ;; ```
                ;;
                ;; Which is a subset from the graph represented by
                ;; `::graph/nodes`.
                (let [graph (update-in graph [::graph/backlinks id-bob] disj id-alice)]
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
                          (assoc ::graph/nodes     {id-alice node-alice})
                          (assoc ::graph/backlinks {id-bob #{id-alice}}))]
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
