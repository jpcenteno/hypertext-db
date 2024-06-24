(ns zettel.graph.node-test
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [clojure.test            :refer [deftest is testing]]
            [zettel.graph.node       :as node]
            [zettel.test.fixtures    :as fixtures]
            [zettel.vault.vault-file :as vault-file]))

(deftest spec
  (let [some-vault-file (fixtures/vault-file)]
    (testing "Sample valid `::node/t` without links nor backlinks"
      (is (s/valid? ::node/t (merge some-vault-file
                                    {::node/links     #{}
                                     ::node/backlinks #{}}))))

    (testing "Sample valid `::node/t` with links and backlinks"
      (is (s/valid? ::node/t (merge some-vault-file
                                    {::node/links     #{(fixtures/id)}
                                     ::node/backlinks #{(fixtures/id)}}))))

    (testing "Links to self are invalid"
      (is (not (s/valid? ::node/t
                         (merge some-vault-file
                                {::node/links     #{(::vault-file/id some-vault-file)}
                                 ::node/backlinks #{}})))))

    (testing "Backlinks from self are invalid"
      (is (not (s/valid? ::node/t
                         (merge some-vault-file
                                {::node/links     #{}
                                 ::node/backlinks #{(::vault-file/id some-vault-file)}})))))))

(deftest vault-file->
  (testing "`vault-file->` returns a node"
    (let [some-vault-file (fixtures/vault-file)
          node            (node/vault-file-> some-vault-file)]

      (testing "that retains every attribute from the input vault-file"
        (is (set/subset? (set some-vault-file)
                         (set node))))

      (testing "without any link"
        (is (empty? (::node/links node))))

      (testing "without any backlink"
        (is (empty? (::node/backlinks node)))))))

(comment (->> (fixtures/vault-file)
              (node/vault-file->)
              (s/explain ::node/t)))
