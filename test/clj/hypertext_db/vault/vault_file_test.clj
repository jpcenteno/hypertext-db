(ns hypertext-db.vault.vault-file-test
  (:require [clojure.test :refer [deftest is testing]]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

(def test-file (File. "iron-clap-syrup-cruise.tar.bz2"))

(deftest file->

  (testing "Sets ::id correctly"
    (testing "When applied to a file with a well formed name"
      (let [file (File. "message-ceiling-tape-hobby.md")
            result (vault-file/file-> file)]
        (is (= file (vault-file/id result))))))

  (testing "Sets `::last-modified-ms`"
    (testing "to `0` when not provided"
      (is (zero? (-> test-file vault-file/file-> ::vault-file/last-modified-ms))))

    (testing "to the provided value when given one"
      (let [last-modified-ms 12345
            result (vault-file/file-> test-file last-modified-ms)]
        (is (= last-modified-ms (::vault-file/last-modified-ms result)))))))
