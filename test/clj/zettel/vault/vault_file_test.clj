(ns zettel.vault.vault-file-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.vault.vault-file :as vault-file]
            [zettel.helpers.tmp :as tmp]
            [failjure.core :as f])
  (:import (java.io File)))

(def test-file-non-existing (File. "iron-clap-syrup-cruise.tar.bz2"))
(def test-file-non-existing-no-extension (File. "among-ready-stereo-casino"))

(deftest file->

  (testing "Sets ::id correctly"
    (testing "When applied to a file with a well formed name"
      (let [file (File. "message-ceiling-tape-hobby.md")
            result (vault-file/file-> file)]
        (is (= (::vault-file/id result)
               [:message :ceiling :tape :hobby])))))

  (testing "Sets ::ext correctly"

    (testing "when applied to a file with a simple extension"
      (let [file (File. "message-ceiling-tape-hobby.md")
            result (vault-file/file-> file)]
        (is (= (::vault-file/ext result) "md"))))

    (testing "When applied to a file with a composite extension"
      (let [input-file   (File. "iron-clap-syrup-cruise.tar.bz2")
            result       (vault-file/file-> input-file)]
        (is (= (::vault-file/ext result) "tar.bz2")))))

  (testing "Sets `::last-modified-ms`"
    (testing "to `0` when the file does not exist"
      ;; This has the nice property that if the file is created later, it will
      ;; have a higher `::last-modified-ms` value.
      (let [file   (File. "/non/existing/thing-agree-early-income.png")
            result (vault-file/file-> file)]
        (is (zero? (::vault-file/last-modified-ms result)))))

    (testing "to the value set in the file-system when the file exists"
      (tmp/with-tmp-dir
        (let [file     (doto (File. tmp/dir "owner-twin-scatter-purity.md")
                         (.createNewFile)
                         (.setLastModified 42))
              result   (vault-file/file-> file)]
          (is (= (::vault-file/last-modified-ms result)
                 42))))))

  (testing "Ignores any parent directories"
    ;; DESIGN DECISION:
    ;; The `vault-file` domain assumes that any file passed ito it resides at
    ;; the top level of the `vault` directory. All directory related information
    ;; will be discarded.
    (let [file   (File. "/path/to/outside-meat-bubble-jealous.md")
          result (vault-file/file-> file)]
      (is (f/ok? result))
      (is (= (::vault-file/id result)
             [:outside :meat :bubble :jealous]))))

  (testing "Returns a `f/failure?`"

    (testing "when applied to an empty filename"
      (is (f/failed? (vault-file/file-> (File. "")))))

    (testing "When applied to a hidden file"
      (is (f/failed? (vault-file/file-> (File. ".arctic-execute-thumb-crater.md")))))

    (testing "when applied to a file with an invalid id string as filename:"
      (testing "No filename, but extension"
        (is (f/failed? (vault-file/file-> (File. ".gitignore")))))
      (testing "More than the specified amount of valid words"
        (is (f/failed? (vault-file/file-> (File. "truck-join-wet-dry-truck-join-wet-dry.md")))))
      (testing "Invalid words"
        (is (f/failed? (vault-file/file-> (File. "some-invalid-id-words.md"))))))

    (testing "when applied to a file with an invalid extension:"

      (testing "Without extension"
        ;; I'm reserving files without extension to store metadata for
        ;; non-markdown files. In any case, files without extension are a weird
        ;; thing.
        (is (f/failed? (vault-file/file-> (File. "arctic-execute-thumb-crater")))))

      (testing "Ending with a dot and no extension"
        (is (f/failed? (vault-file/file-> (File. "arctic-execute-thumb-crater.")))))

      (testing "Ending with two dots and no extension"
        (is (f/failed? (vault-file/file-> (File. "arctic-execute-thumb-crater..")))))

      (testing "Extension ending with a dot"
        (is (f/failed? (vault-file/file-> (File. "arctic-execute-thumb-crater.txt."))))))))
