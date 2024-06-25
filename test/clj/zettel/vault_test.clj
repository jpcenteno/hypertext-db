(ns zettel.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.helpers.tmp :as tmp]
            [zettel.test.fixtures :as fixtures]
            [zettel.vault :as vault]
            [zettel.vault.vault-file :as vault-file]
            [clojure.spec.alpha :as s])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Tests                                                                  ║
; ╚════════════════════════════════════════════════════════════════════════╝

(deftest dir->
  (testing "Creates a ::zettel.vault instance"
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
       (let [vault        (vault/dir-> tmp/dir)
             vault-file-1 (fixtures/vault-file-that-exists tmp/dir {::vault-file/id (File. "filename-1.md")})
             vault-file-2 (fixtures/vault-file-that-exists tmp/dir {::vault-file/id (File. "filename-2.md")})
             result       (vault/list-vault-files vault)]
         (is (= 2 (count result)))
         (is (contains? result vault-file-1))
         (is (contains? result vault-file-2))))))

  (testing "Lists files under directories"
    (is (tmp/with-tmp-dir
          (let [_subdir    (doto (File. tmp/dir "subdir")
                             (.mkdir))
                attrs      {::vault-file/id (File. "subdir/test-file-in-subdirectory.md")}
                vault-file (fixtures/vault-file-that-exists tmp/dir attrs)
                result     (-> tmp/dir vault/dir-> vault/list-vault-files)]
            (contains? result vault-file)))))

  (testing "Lists hidden filenames"
    (is (tmp/with-tmp-dir
          (let [attrs    {::vault-file/id (File. ".im-a-hidden-test-file.exe")}
                expected (fixtures/vault-file-that-exists tmp/dir attrs)
                result   (-> tmp/dir vault/dir-> vault/list-vault-files)]
            (is (contains? result expected))))))

  (testing "Ignores directories"
    (tmp/with-tmp-dir
      (let [vault (vault/dir-> tmp/dir)
            _     (doto (File. tmp/dir "firm-popular-carpet-tree")
                    (.mkdir))]
        (is (empty? (vault/list-vault-files vault)))))))
