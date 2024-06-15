(ns zettel.vault-test
  (:require [clojure.test :refer [deftest is testing]]
            [zettel.helpers.tmp :as tmp]
            [zettel.vault :as vault]
            [clojure.spec.alpha :as s]))

(deftest dir->
  (testing "Creates a ::zettel.vault instance"
    (tmp/with-tmp-dir
      (is (s/valid? ::vault/t (vault/dir-> tmp/dir))))))

(comment
  (deftest get-file-list
    (testing "Empty dir returns empty collection"
      (tmp/with-tmp-dir
        (let [vault (vault/dir-> tmp/dir)
              result (vault/get-file-list vault)]
          (is (empty? result)))))
    (testing "Lists files in vault where filename matches an ID"
      (tmp/with-tmp-dir
        (let [vault (vault/dir-> tmp/dir)
              result (vault/get-file-list vault)]
          (is (empty? result)))))))
