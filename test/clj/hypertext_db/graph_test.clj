(ns hypertext-db.graph-test
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test            :refer [deftest is testing]]
            [hypertext-db.graph            :as graph]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.helpers.tmp      :as tmp]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file]
            [hypertext-db.graph.backlinks-impl :as backlinks]
            [hypertext-db.test.fixtures :as fixtures]
            [hypertext-db.test.simple-node-parser :as simple-parser])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Test data                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef empty-graph :ret ::graph/t)
(defn- empty-graph []
  (-> (System/getProperty "java.io.tmpdir") (java.io.File.) vault/dir-> graph/vault->))

;; For most of the tests we will be using a simple graph with two nodes,
;; `node-a` and `node-b`, where `node-a` links to `node-b`, but `node-b`
;; does not link to `node-a`.
;;
;; [A] --> [B]

(def ^:private node-b-id (fixtures/id "node-b.file"))
(def ^:private node-b    (fixtures/node {::vault-file/id node-b-id ::node/links #{}}))
(def ^:private node-a-id (fixtures/id "node-a.file"))
(def ^:private node-a    (fixtures/node {::vault-file/id node-a-id ::node/links #{node-b-id}}))

(def ^:private node-a-without-links
  "A modified `node-a` without the link to `node-b`. Used for some of the
  tests."
  (assoc node-a ::node/links #{}))

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

  (testing "In the `::graph/nodes` map"

    (testing "every key should match the node's id."
      ;; The map data structure was chosen for `::graph/nodes` to serve as an
      ;; index by node id, then, every key must match the value's node id.

      (testing "A `::graph/nodes` index with a  mismatching key"
        ;; In this example, the value for `node-a-id` has a different node id:
        (let [graph (assoc (empty-graph) ::graph/nodes {node-a-id node-b})]

          (testing "is invalid"
            (is (not (s/valid? ::graph/t graph)))))

        (testing "can be fixed by replacing the value with the correct node"
          (let [graph (assoc-in (empty-graph) [::graph/nodes node-a-id] node-a-without-links)]
            (is (s/valid? ::graph/t graph))))

        (testing "can be fixed by removing the offending element from `::graph/nodes`"
          (let [graph (update (empty-graph) ::graph/nodes dissoc node-a-id)]
            (is (s/valid? ::graph/t graph))))))

    (testing "every link should have a reciprocal mapping in `::graph/backlinks`:"
      ;; The `::graph/backlinks` map is a reverse-index for
      ;; the links declared in the `::graph/nodes` map. It's fundamental
      ;; that every link described in the later is reciprocated in the
      ;; former.

      (testing "A graph with a missing backlink"
        ;; In this example, the `::graph/nodes` map describes the following
        ;; graph:
        ;;
        ;; [A] -> [B]
        ;;
        ;; While `::graph/backlinks` is empty.
        (let [graph (merge (empty-graph)
                           {::graph/nodes     {node-a-id node-a
                                               node-b-id node-b}
                            ::graph/backlinks {}})]

          (testing "is invalid"
            (is (not (s/valid? ::graph/t graph))))

          (testing "can be fixed by adding the missing backlink"
            ;; To fix the broken graph, we can ad the missing backlink.
            (let [id-from node-a-id
                  id-to   node-b-id
                  graph   (update graph ::graph/backlinks backlinks/add id-from id-to)]
              (is (s/valid? ::graph/t graph)))))))

    (testing "Nodes are allowed to have links pointing to unknown nodes"
      ;; Nodes are allowed to contain link ids to nodes that are not included in
      ;; the `::graph/nodes` map. The rationale is:
      ;;
      ;; - The node domain is unconcerned with the other nodes included in the
      ;;   graph.
      ;; - A node's file might link to a node that does not exist. This is an
      ;;   external problem.
      ;; - A node might link to another node that hasn't been added to the graph
      ;;   yet.
      ;;
      ;; The only condition is that those links must be kept in the
      ;; `::graph/backlinks` map in case that the missing nodes are added later.

      (testing "A graph where a node has a link to a missing node and it's corresponding backlink"
        ;; In this example, `graph` models a graph where a node `[A]` links to
        ;; an unknown node `(B)`.
        ;;
        ;; [A] --> (B)
        (let [graph (-> (empty-graph)
                        (assoc ::graph/nodes     {node-a-id node-a})
                        (assoc ::graph/backlinks {node-b-id #{node-a-id}}))]
          (testing "Is valid"
            (is (s/valid? ::graph/t graph)))

          (testing "Is invalid if we remove the reverse link from `::graph/backlinks`"
            (let [invalid-graph (update-in graph [::graph/backlinks node-b-id] disj node-a-id)]
              (is (not (s/valid? ::graph/t invalid-graph)))))))))

  (testing "Links from ::graph/backlinks"

    (testing "must come from node id's contained by `::graph/nodes`:"

      (testing "A graph with a backlink comming from a node id which is not present in `::graph/nodes`"
        ;; In this example we have a graph where `::graph/nodes` can be
        ;; represented as:
        ;;
        ;; [B]
        ;;
        ;; While `::graph/backlinks` can be represented as:
        ;;
        ;; [B] <-- [A]
        ;;
        ;; Here, the `[B] <-- [A]` backlink comes from nonwhere!
        (let [invalid-graph (merge (empty-graph)
                                   {::graph/nodes     {node-b-id node-b}
                                    ::graph/backlinks {node-b-id #{node-a-id}}})]

          (testing "is invalid"
            (is (not (s/valid? ::graph/t invalid-graph))))

          (testing "can be fixed by adding a node that reflects the backlink"
            ;; We can insert a node [A] that links to the node [B] in order to
            ;; complete the reciprocal link. Now `::graph/nodes` can be
            ;; represented as:
            ;;
            ;; [A] --> [B]
            ;;
            ;; Which matches the links described by `::graph/backlinks`
            (let [graph (assoc-in invalid-graph [::graph/nodes node-a-id] node-a)]
              (is (s/valid? ::graph/t graph)))))))

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

      (testing "A graph with a non-reciprocated backlink from an existing node"
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
        (let [graph (merge (empty-graph)
                           {::graph/nodes     {node-a-id node-a-without-links
                                               node-b-id   node-b}
                            ::graph/backlinks {node-b-id #{node-a-id}}})]

          (testing "is invalid"
            (is (not (s/valid? ::graph/t graph))))

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
            (let [graph (update graph ::graph/backlinks backlinks/remove-link node-a-id node-b-id)]
              (is (s/valid? ::graph/t graph)))))))))

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

(deftest conj-node

  (testing "Inserting a leaf node"
    (let [graph (-> (empty-graph) (graph/conj-node node-b))]
      (testing "assocs the node to `::graph/nodes`"
        (is (= node-b (get-in graph [::graph/nodes node-b-id]))))
      (testing "Is idempotent"
        (is (= graph (graph/conj-node graph node-b))))))

  (testing "Inserting a node with a link"
    (let [graph (-> (empty-graph) (graph/conj-node node-a))]
      (testing "assocs the node to `::graph/nodes`"
        (is (= node-a (get-in graph [::graph/nodes node-a-id]))))
      (testing "inserts the mirroring backlink"
        (is (contains? (get-in graph [::graph/backlinks node-b-id]) node-a-id)))))

  (testing "Inserting a changed version of a node"
    (is (let [node  (fixtures/node)
              node' (fixtures/node {::vault-file/id (node/id node)})
              graph (empty-graph)]
          (is (= (-> graph (graph/conj-node node'))
                 (-> graph (graph/conj-node node) (graph/conj-node node'))))))))

(declare conj-node-is-idempotent)
(defspec conj-node-is-idempotent 10
  (prop/for-all
   [node (s/gen ::node/t)]
   (is (= (-> (empty-graph) (graph/conj-node node))
          (-> (empty-graph) (graph/conj-node node) (graph/conj-node node))))))

(declare conj-node-is-able-to-perform-updates)
(defspec conj-node-is-able-to-perform-updates 10
  (prop/for-all
   [node   (s/gen ::node/t)]
   (is (let [node' (fixtures/node {::vault-file/id (node/id node)})
             graph (empty-graph)]
         (is (= (graph/conj-node graph node')
                (-> graph (graph/conj-node node) (graph/conj-node node'))))))))

(deftest disj-node

  (testing "Returns the graph without"
    (testing "A node that was not contained by the graph in the first place"
      (let [graph (empty-graph)]
        (is (= graph (graph/disj-node graph node-a)))
        (is (= graph (graph/disj-node graph node-b)))))

    (testing "A node without links"
      (let [graph-1 (empty-graph)
            graph-2 (graph/conj-node graph-1 node-b)]
        (is (= graph-1 (graph/disj-node graph-2 node-b)))))

    (testing "A node with links"
      (let [graph-1 (empty-graph)
            graph-2 (graph/conj-node graph-1 node-a)]
        (is (= graph-1 (graph/disj-node graph-2 node-a))))))

  (testing "When provided an altered version of a node, removes the contained node with the same id"
    (is (let [graph (empty-graph)
              node  (fixtures/node)
              node' (fixtures/node {::vault-file/id (node/id node)})]
          (= graph
             (-> graph (graph/conj-node node) (graph/disj-node node')))))))

(declare disj-node-is-idempotent)
(defspec disj-node-is-idempotent 10
  (prop/for-all
   [node (s/gen ::node/t)]
   (is (let [graph (graph/conj-node (empty-graph) node)]
         (= (-> graph (graph/disj-node node))
            (-> graph (graph/disj-node node) (graph/disj-node node)))))))

(declare disj-node-succeeds-when-provided-an-altered-version-of-a-node)
(defspec disj-node-succeeds-when-provided-an-altered-version-of-a-node
  (prop/for-all
   [node (s/gen ::node/t)]
   (is (let [graph (empty-graph)
             node' (fixtures/node {::vault-file/id (node/id node)})]
         (= graph
            (-> graph (graph/conj-node node) (graph/disj-node node')))))))

(deftest test-add-node-from-vault-file

  (testing "When applied to an empty graph and a blank file, it returns a graph with a new node"
    (is (let [vault         (first (gen/sample (s/gen ::vault/t) 1))
              input-graph   (graph/vault-> vault)
              vault-file    (first (gen/sample (s/gen ::vault-file/t) 1))
              expected-id   (::vault-file/id vault-file)
              result        (graph/add-node-from-vault-file input-graph vault-file)]
          (testing "increasing the node count"
            (is (= (inc (graph/node-count input-graph))
                   (graph/node-count result))))
          (testing "with the correct id"
            (is (graph/contains-node? result expected-id)))
          (testing "equal to the result applying node/vault-file-> to the input"
            (is (= (node/vault-file-> vault-file)
                   (graph/get-node result expected-id)))))))

  (testing "Provided a graph with a simple custom parser"
    (is (let [graph           (-> (fixtures/vault) graph/vault-> (graph/set-parsers [simple-parser/parser]))]

          (testing "and a `::vault-file/t` that the parser CAN parse"
            (is (let [links       #{(File. "foo.md") (File. "bar.png")}
                      vault-file  (simple-parser/create-vault-file graph links)
                      id          (::vault-file/id vault-file)
                      graph-after (graph/add-node-from-vault-file graph vault-file)]

                  (testing "Returns a graph containing a node"
                    (testing  "with the same id"
                      (is (graph/contains-node? graph-after id)))

                    (testing "with all the links passed to the fixture"
                      (is (= links (-> graph-after (graph/get-node id) ::node/links))))))))))))

;; FIXME move this to a corresponding namespace.
(s/fdef hash-map-subset?
  :args (s/cat :m1 map? :m2 map?)
  :ret  boolean?)
(defn- hash-map-subset? [m1 m2]
  (set/subset? (set m1) (set m2)))

(deftest test-batch-sync-graph-with-vault

  (testing "Adds nodes after creating new files:"

    (testing "Returns an empty graph when no new files are created"
      (let [graph-arg (fixtures/graph-empty)]
        (is (= graph-arg (graph/batch-sync-graph-with-vault graph-arg)))))

    (testing "Adds a single node to an empty graph after creating a new file"
      (let [graph-arg (fixtures/graph-empty)
            vault-file  (fixtures/vault-file-that-exists graph-arg)
            graph-ret (graph/batch-sync-graph-with-vault graph-arg)]
        (is (graph/contains-node? graph-ret (::vault-file/id vault-file)))
        (is (= 1 (graph/node-count graph-ret)))))

    (testing "Adds two nodes to an empty graph after creating two new files"
      (let [graph-arg    (fixtures/graph-empty)
            vault-file-1 (fixtures/vault-file-that-exists graph-arg)
            vault-file-2 (fixtures/vault-file-that-exists graph-arg)
            graph-ret    (graph/batch-sync-graph-with-vault graph-arg)]
        (is (graph/contains-node? graph-ret (::vault-file/id vault-file-1)))
        (is (graph/contains-node? graph-ret (::vault-file/id vault-file-2)))
        (is (= 2 (graph/node-count graph-ret)))))

    (testing "Adds a node to a non-empty graph after creating another file"
      (let [graph-arg     (fixtures/graph-with-nodes-that-exist-in-vault)
            vault-file-1  (-> graph-arg ::graph/nodes vals first)
            vault-file-2  (fixtures/vault-file-that-exists graph-arg)
            graph-ret     (graph/batch-sync-graph-with-vault graph-arg)]
        (is (graph/contains-node? graph-ret (::vault-file/id vault-file-2)))
        (is (= (graph/get-node graph-arg (::vault-file/id vault-file-1))
               (graph/get-node graph-ret (::vault-file/id vault-file-1))))
        (is (= 2 (graph/node-count graph-ret)))))

    (testing "Adds a node to an empty graph with a custom parser"
      (let [graph-pre       (graph/set-parsers (fixtures/graph-empty)  [simple-parser/parser])
            vault-file-from (simple-parser/create-vault-file graph-pre #{(File. "to.png")})
            graph-post      (graph/batch-sync-graph-with-vault graph-pre)]
        (is (graph/contains-node? graph-post (::vault-file/id vault-file-from))
            "The returned graph should contain the node associated to the new file")
        (is (-> graph-post (graph/get-node (::vault-file/id vault-file-from)) ::node/links (contains? (File. "to.png")))
            "The custom parser should include the link from the file (proving that it used the parser)")
        (is (= 1 (graph/node-count graph-post))
            "The resulting graph shall not contain any other node."))))

  (testing "Does nothing if no new files are created"
    (let [graph-initial (fixtures/graph-with-nodes-that-exist-in-vault)]
      (is (= graph-initial
             (graph/batch-sync-graph-with-vault graph-initial))
          "Return value must be equal to the argument graph")))

  (testing "Updates nodes after updating files:"

    (testing "Updates a single node"
      (let [graph-initial      (fixtures/graph-with-nodes-that-exist-in-vault)
            vault-file-initial (-> graph-initial ::graph/nodes vals first)
            vault-file-after   (fixtures/vault-file-that-exists
                                graph-initial
                                (assoc vault-file-initial ::vault-file/last-modified-ms 1))
            graph-after   (graph/batch-sync-graph-with-vault graph-initial)]
        (is (hash-map-subset? vault-file-after (graph/get-node graph-after (::vault-file/id vault-file-initial))))
        (is (= 1 (graph/node-count graph-after))))))

  (testing "Removes nodes after deleting files:"
    (testing "Remove the sole node from a graph"
      (let [graph-pre  (fixtures/graph-with-nodes-that-exist-in-vault)
            vault-file (-> graph-pre ::graph/nodes vals first)
            _          (doto (#'fixtures/vault-and-vault-file->java-file graph-pre vault-file)
                         .delete)
            graph-post (graph/batch-sync-graph-with-vault graph-pre)]
        (is (zero? (graph/node-count graph-post)))))

    (testing "Removes two files, keeps one"
      (let [vault      (fixtures/vault)
            file-1     (fixtures/vault-file-that-exists vault)
            file-2     (fixtures/vault-file-that-exists vault)
            file-3     (fixtures/vault-file-that-exists vault)
            graph-pre  (-> vault graph/vault-> graph/batch-sync-graph-with-vault)]
        (fixtures/vault-file-delete graph-pre file-1)
        (fixtures/vault-file-delete graph-pre file-2)
        (let [graph-post (graph/batch-sync-graph-with-vault graph-pre)]
          (is (not (graph/contains-node? graph-post (::vault-file/id file-1))))
          (is (not (graph/contains-node? graph-post (::vault-file/id file-2))))
          (is (graph/contains-node? graph-post (::vault-file/id file-3)))
          (is (= 1 (graph/node-count graph-post)))))))

  (testing "Creating and deleting files:"
    (testing "Creates and deletes multiple files:"
      (let [vault     (fixtures/vault)
            file-1    (fixtures/vault-file-that-exists vault)
            file-2    (fixtures/vault-file-that-exists vault)
            file-3    (fixtures/vault-file-that-exists vault)
            file-4    (fixtures/vault-file-that-exists vault)
            graph-pre (-> vault graph/vault-> graph/batch-sync-graph-with-vault)
            file-5    (fixtures/vault-file-that-exists vault)
            file-6    (fixtures/vault-file-that-exists vault)]
        (fixtures/vault-file-delete graph-pre file-1)
        (fixtures/vault-file-delete graph-pre file-2)
        (let [graph-post (graph/batch-sync-graph-with-vault graph-pre)]
          (is (not (graph/contains-node? graph-post (::vault-file/id file-1))))
          (is (not (graph/contains-node? graph-post (::vault-file/id file-2))))
          (is (= (graph/get-node graph-pre (::vault-file/id file-3)) (graph/get-node graph-post (::vault-file/id file-3))))
          (is (= (graph/get-node graph-pre (::vault-file/id file-4)) (graph/get-node graph-post (::vault-file/id file-4))))
          (is (graph/contains-node? graph-post (::vault-file/id file-5)))
          (is (graph/contains-node? graph-post (::vault-file/id file-6)))
          (is (= 4 (graph/node-count graph-post))))))))
