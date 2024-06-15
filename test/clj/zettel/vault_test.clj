(ns zettel.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.helpers.tmp :as tmp]
            [zettel.id :as id]
            [zettel.vault :as vault]
            [zettel.vault.vault-file :as vault-file]
            [clojure.spec.alpha :as s])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Fixtures                                                               ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- random-vault-file-name [extension]
  (str (id/->str (id/random)) "." extension))

(defn- create-vault-file [vault-directory extension]
  (let [filename (random-vault-file-name extension)
        file     (doto (File. vault-directory filename)
                   (.createNewFile))]
    (vault-file/file-> file)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Tests                                                                  ║
; ╚════════════════════════════════════════════════════════════════════════╝

(deftest dir->
  (testing "Creates a ::zettel.vault instance"
    (tmp/with-tmp-dir
      (is (s/valid? ::vault/t (vault/dir-> tmp/dir))))))

(deftest get-file-list

  (testing "Returns an empty collection when the vault directory is empty"
    (tmp/with-tmp-dir
      (let [vault (vault/dir-> tmp/dir)]
        (is (empty? (vault/get-file-list vault))))))

  (testing "Returns a collection of all the vault-files in the top level directory"
    (tmp/with-tmp-dir
      (let [vault        (vault/dir-> tmp/dir)
            vault-file-1 (create-vault-file tmp/dir "md")
            vault-file-2 (create-vault-file tmp/dir "md")
            result       (vault/get-file-list vault)]
        (is (= 2 (count result)))
        (is (contains? result vault-file-1))
        (is (contains? result vault-file-2))))))
