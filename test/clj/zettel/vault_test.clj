(ns zettel.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.helpers.tmp :as tmp]
            [zettel.test.fixtures :as fixtures]
            [zettel.id :as id]
            [zettel.vault :as vault]
            [zettel.vault.vault-file :as vault-file]
            [clojure.spec.alpha :as s])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Fixtures                                                               ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- random-vault-file-name [extension]
  (str (id/->str (fixtures/id)) "." extension))

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

(deftest list-vault-files

  (testing "Returns an empty collection when the vault directory is empty"
    (tmp/with-tmp-dir
      (let [vault (vault/dir-> tmp/dir)]
        (is (empty? (vault/list-vault-files vault))))))

  (testing "Returns a collection of all the vault-files in the top level directory"
    (tmp/with-tmp-dir
      (let [vault        (vault/dir-> tmp/dir)
            vault-file-1 (create-vault-file tmp/dir "md")
            vault-file-2 (create-vault-file tmp/dir "md")
            result       (vault/list-vault-files vault)]
        (is (= 2 (count result)))
        (is (contains? result vault-file-1))
        (is (contains? result vault-file-2)))))

  (testing "Ignores..."

    (testing "directories"
      (tmp/with-tmp-dir
        (let [vault (vault/dir-> tmp/dir)
              _     (doto (File. tmp/dir "firm-popular-carpet-tree")
                      (.mkdir))]
          (is (empty? (vault/list-vault-files vault))))))

    (testing "Well named files under directories"
      (tmp/with-tmp-dir
        (let [vault  (vault/dir-> tmp/dir)
              subdir (doto (File. tmp/dir "firm-popular-carpet-tree")
                       (.mkdir))]
          (create-vault-file subdir "md")
          (is (empty? (vault/list-vault-files vault))))))

    (testing "Hidden, but otherwise well formed filenames"
      (tmp/with-tmp-dir
        (let [vault (vault/dir-> tmp/dir)]
          (doto (File. tmp/dir ".firm-popular-carpet-tree.md")
            (.createNewFile))
          (is (empty? (vault/list-vault-files vault))))))

    (testing "Files named after an id, but without extension"
      (tmp/with-tmp-dir
        (let [vault (vault/dir-> tmp/dir)]
          (doto (File. tmp/dir "elbow-turkey-tank-thank")
            (.createNewFile))
          (is (empty? (vault/list-vault-files vault))))))))
