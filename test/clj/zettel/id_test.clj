(ns zettel.id-test
  (:require [zettel.id :as id]
            [zettel.test.fixtures :as fixtures]
            [clojure.test :refer [deftest is testing]]
            [failjure.core :as f]))

(deftest str->
  (testing "Applied to a valid id-string"
    (let [input    "seven-like-derive-shove"
          expected [:seven :like :derive :shove]
          result   (id/str-> input)]
      (testing "returns a successful value"
        (is (f/ok? result)))
      (testing "returns the expected id"
        (is (= result expected)))))

  (testing "Unhappy path:"
    (testing "Returns a failure value when applied to"
      (testing "an empty string"
        (is (f/failed? (id/str-> ""))))
      (testing "a string with invalid words"
        (is (f/failed? (id/str-> "some-invalid-id-words"))))
      (testing "a string with more than the specified amount of valid words"
        (is (f/failed? (id/str-> "excuse-vicious-desk-siege-accuse-truth-process-divert")))))))

(deftest ->str
  (testing "Round trip"
    (let [id (fixtures/id)]
      (is (= (-> id id/->str id/str->) id)))))

(deftest id-string?
  (testing "Applied to a valid id-string"
    (is (id/id-string? "truck-join-wet-dry")))
  (testing "Applied to an empty string"
    (is (not (id/id-string? ""))))
  (testing "Applied to a string with more than the specified amount of valid words"
    (is (not (id/id-string? "truck-join-wet-dry-truck-join-wet-dry"))))
  (testing "Applied to invalid words"
    (is (not (id/id-string? "some-invalid-id-words"))))
  (testing "Applied to a non-string value"
    (is (not (id/id-string? :some-keyword)))))
