(ns hypertext-db.delete-this-test
  (:require [clojure.test :refer [deftest is testing]]))

(deftest on-pourpose-failing-test
  (testing "This test will fail on purpose just to check that the CI is working"
    (is (= 5 (+ 2 2)))))
