(ns hypertext-db.graph.parser-test
  (:require [clojure.test                                         :refer [deftest is testing]]
            [clojure.spec.alpha                                   :as s]
            [clojure.spec.gen.alpha                               :as gen]
            [hypertext-db.graph.node                              :as node]
            [hypertext-db.graph.parser                            :as parser]
            [hypertext-db.test.fixtures                           :as fixtures]
            [hypertext-db.test.simple-node-parser                 :as simple-parser]
            [hypertext-db.vault.vault-file                        :as vault-file])
  (:import (java.io File)))

(deftest test-parse
  (testing "Provided an empty `::parser/parser-chain` and a random `::vault-file/t`"
    (is (let [vault      (fixtures/vault)
              vault-file (fixtures/vault-file)]
          (testing "falls back to `node/vault-file->`"
            (is (= (node/vault-file-> vault-file)
                   (parser/parse [] vault-file vault)))))))

  (testing "Provided a parser in the chain"
    (is (let [vault           (fixtures/vault)
              links           #{(File. "link-1.md") (File. "link-2.png")}
              vault-file-can  (simple-parser/create-vault-file vault links)
              vault-file-cant (fixtures/vault-file)
              parsers         [simple-parser/parser]]

          (testing "and a `::vault-file/t` that the parser CAN'T parse"
            (testing "it falls back to `node/vault-file->`"
              (is (= (node/vault-file-> vault-file-cant)
                     (parser/parse parsers vault-file-cant vault)))))

          (testing "and a `::vault-file/t` that the file parser CAN parse"
            (testing "it returns the same result as evaluating the parser's `::parser/parse-fn`"
              (is (let [expected ((::parser/parse-fn simple-parser/parser) vault-file-can vault)
                        result   (parser/parse parsers vault-file-can vault)]
                    (is (= expected result))))))))))
