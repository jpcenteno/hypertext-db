(ns hypertext-db.vault.vault-file-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [hypertext-db.vault.vault-file   :as vault-file]
            [hypertext-db.helpers.vault-file :as h.vault-file]))

(defn valid-relative-path? [x]
  (s/valid? ::vault-file/relative-path x))

(deftest test-helper-replace-ext
  (is (= "a.ext"      (#'h.vault-file/replace-ext "a"          "ext")))
  (is (= "a.ext"      (#'h.vault-file/replace-ext "a.b"        "ext")))
  (is (= "a.ext"      (#'h.vault-file/replace-ext "a.b.c"      "ext")))
  (is (= "/dir/a.ext" (#'h.vault-file/replace-ext "/dir/a"     "ext")))
  (is (= "/dir/a.ext" (#'h.vault-file/replace-ext "/dir/a.b"   "ext")))
  (is (= "/dir/a.ext" (#'h.vault-file/replace-ext "/dir/a.b.c" "ext")))
  (is (= "/x.y/a.ext" (#'h.vault-file/replace-ext "/x.y/a"     "ext")))
  (is (= "/x.y/a.ext" (#'h.vault-file/replace-ext "/x.y/a.b"   "ext")))
  (is (= "/x.y/a.ext" (#'h.vault-file/replace-ext "/x.y/a.b.c" "ext"))))

(deftest relative-file-spec
  (is (valid-relative-path?      "a/b.c")     "Valid path.")
  (is (valid-relative-path?      "a/.b.c")    "Hidden files are legit.")
  (is (not (valid-relative-path? ""))         "No empty paths!")
  (is (not (valid-relative-path? "/foo/bar")) "No absolute paths!")
  (is (not (valid-relative-path? "a/../b"))   "No un-normalized paths!")
  (is (not (valid-relative-path? "../a"))     "No paths outside base dir!"))

(deftest file->

  (testing "Sets ::id correctly"
    (testing "When applied to a file with a well formed name"
      (let [file "message-ceiling-tape-hobby.md"
            result (vault-file/file-> file)]
        (is (= file (vault-file/id result))))))

  (testing "Sets `::last-modified-ms`"
    (testing "to `0` when not provided"
      (is (zero? (-> "some filename.txt" vault-file/file-> ::vault-file/last-modified-ms))))

    (testing "to the provided value when given one"
      (let [last-modified-ms 12345
            result (vault-file/file-> "some filename.txt" last-modified-ms)]
        (is (= last-modified-ms (::vault-file/last-modified-ms result)))))))
