(ns zettel.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.vault :as vault]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(def ^:dynamic *dir* :dir-not-initialized)

(defn- rm
  [file]
  (run! io/delete-file (reverse (file-seq file))))

(defn- random-tmp-dir-file []
  (io/file (System/getProperty "java.io.tmpdir")
           (str "zettel-test-" (int (rand Integer/MAX_VALUE)))))

(defn- create-tmp-dir []
  (loop [attempts-left 10]
    (if (zero? attempts-left)
      (throw (Exception. "Failed to create tmp dir for the vault"))
      (let [dir (random-tmp-dir-file)]
        (if (.mkdir dir)
          dir
          (recur (dec attempts-left)))))))

(defmacro with-tmp-dir
  [body]
  `(binding [*dir* (create-tmp-dir)]
     (try (~@body)
          (finally (rm *dir*)))))

(deftest dir->
  (testing "Creates a ::zettel.vault instance"
    (with-tmp-dir
      (is (s/valid? ::vault/t (vault/dir-> *dir*))))))

(comment
  (deftest get-file-list
    (testing "Empty dir returns empty collection"
      (with-tmp-dir
        (let [vault (vault/dir-> *dir*)
              result (vault/get-file-list vault)]
          (is (empty? result)))))
    (testing "Lists files in vault where filename matches an ID"
      (with-tmp-dir
        (let [vault (vault/dir-> *dir*)
              result (vault/get-file-list vault)]
          (is (empty? result)))))))
