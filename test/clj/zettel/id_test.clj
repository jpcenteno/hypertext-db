(ns zettel.id-test
  (:require [zettel.id :as id]
            [clojure.test :refer [deftest is testing]]))

(deftest ->str
  (testing "Round trip"
    (let [id (id/random)]
      (is (= (-> id id/->str id/str->) id)))))
