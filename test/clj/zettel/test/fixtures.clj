(ns zettel.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [zettel.id               :as id]
            [zettel.vault.vault-file :as vault-file]))

(defn- generate-one
  [spec]
  (first (gen/sample (s/gen spec) 1)))

(s/fdef id :ret ::id/t)
(defn id []
  (generate-one ::id/t))

(s/fdef vault-file :ret ::vault-file/t)
(defn vault-file []
  (generate-one ::vault-file/t))
