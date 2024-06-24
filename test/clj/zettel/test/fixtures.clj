(ns zettel.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [zettel.id               :as id]
            [zettel.vault.vault-file :as vault-file]))

(defn id []
  (first (gen/sample (s/gen ::id/t) 1)))

(defn vault-file)
