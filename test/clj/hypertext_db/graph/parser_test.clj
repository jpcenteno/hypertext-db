(ns hypertext-db.graph.parser-test
  (:require [clojure.test                                         :refer [deftest is testing]]
            [clojure.spec.alpha                                   :as s]
            [clojure.spec.gen.alpha                               :as gen]
            [hypertext-db.graph.node                              :as node]
            [hypertext-db.graph.parser                            :as parser]
            [hypertext-db.test.fixtures                           :as fixtures]
            [hypertext-db.test.node-parsers.link-list-node-parser :as test-parser]
            [hypertext-db.vault.vault-file                        :as vault-file])
  (:import (java.io File)))

(deftest test-parse
  (testing "Provided an empty `parser-chain`"
    (is (let [vault      (fixtures/vault)
              vault-file (fixtures/vault-file)]
          (testing "Returns the same value as `node/vault-file->`"
            (is (= (node/vault-file-> vault-file)
                   (parser/parse [] vault-file vault)))))))

  (testing "Provided a parser in the chain"
    (is (let [vault           (fixtures/vault)
              vault-file-cant (fixtures/vault-file)
              links           #{(File. "link-1.md") (File. "link-2.png")}
              vault-file-can  (test-parser/create-vault-file vault links)
              parsers         [test-parser/parser]]

          (testing "And a `vault-file` that the parser CAN'T parse"
            (testing "it returns the same value as `node/vault-file->`"
              (is (= (node/vault-file-> vault-file-cant)
                     (parser/parse parsers vault-file-cant vault)))))

          (testing "And a `vault-file` that the file parser CAN parse"
            (is (let [result (parser/parse parsers vault-file-can vault)]

                  (testing "it returns the same value as the parser's `::parser/parse-fn`"
                    (is (= ((::parser/parse-fn test-parser/parser) vault-file-can vault)
                           result)))

                  (testing "with all the links passed to the fixture"
                    (is (= links (::node/links result)))))))))))
