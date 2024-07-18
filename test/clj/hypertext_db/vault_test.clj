(ns hypertext-db.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [hypertext-db.helpers.tmp :as tmp]
            [hypertext-db.test.fixtures :as fixtures]
            [hypertext-db.vault :as vault]
            [hypertext-db.vault.vault-file :as vault-file]
            [hypertext-db.helpers.vault-file :as helpers.vault-file]
            [clojure.spec.alpha :as s])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Tests                                                                  ║
; ╚════════════════════════════════════════════════════════════════════════╝

(deftest dir->
  (testing "Creates a ::hypertext-db.vault instance"
    (tmp/with-tmp-dir
      (is (s/valid? ::vault/t (vault/dir-> tmp/dir))))))

(deftest list-vault-files

  (testing "Returns an empty collection when the vault directory is empty"
    (tmp/with-tmp-dir
      (let [vault (vault/dir-> tmp/dir)]
        (is (empty? (vault/list-vault-files vault))))))

  (testing "Returns a collection of all the vault-files in the top level directory"
    (is
     (tmp/with-tmp-dir
       (let [vault        (fixtures/vault)
             vault-files  (helpers.vault-file/generate-distinct 2 {:write-to-this-vault vault})
             result       (vault/list-vault-files vault)]
         (is (= 2 (count result)))
         (is (contains? result (vault-files 0)))
         (is (contains? result (vault-files 1)))))))

  (testing "Lists files under directories"
    (tmp/with-tmp-dir
      (let [vault      (fixtures/vault)
            attrs      {::vault-file/id (File. "subdir/test-file-in-subdirectory.md")}
            vault-file (fixtures/vault-file-that-exists vault attrs)]
        (is (contains? (vault/list-vault-files vault) vault-file)))))

  (testing "Lists hidden filenames"
    (is (tmp/with-tmp-dir
          (let [vault      (fixtures/vault)
                attrs      {::vault-file/id (File. ".im-a-hidden-test-file.exe")}
                vault-file (fixtures/vault-file-that-exists vault attrs)]
            (is (contains? (vault/list-vault-files vault) vault-file))))))

  (testing "Ignores directories"
    (tmp/with-tmp-dir
      (let [vault (vault/dir-> tmp/dir)
            _     (doto (File. tmp/dir "firm-popular-carpet-tree")
                    (.mkdir))]
        (is (empty? (vault/list-vault-files vault)))))))

(deftest test-slurp-vault-file
  (testing "Reads content from a vault file"
    (is (let [vault      (fixtures/vault)
              vault-file (fixtures/vault-file)
              file         (File. (::vault/dir vault) (-> vault-file ::vault-file/id str))]
          (spit file "Some text")
          (is (= "Some text" (vault/slurp-vault-file vault vault-file)))))))

(deftest test-contains-absolute-file?
  (testing "Absolute file within the vault directory"
    (let [vault         (fixtures/vault)
          absolute-file (-> (helpers.vault-file/generate-distinct 1)
                            first
                            (helpers.vault-file/java-file vault))]
      (is (vault/contains-absolute-file? vault absolute-file))))

  (testing "Absolute file outside the vault directory"
    (is (not (vault/contains-absolute-file?
              (fixtures/vault)
              (File. "/outside/the/vault.xyz"))))))
