(ns zettel.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [zettel.id               :as id]
            [zettel.vault.vault-file :as vault-file]))

(s/def ::vault-file/id
  (s/with-gen ::vault-file/id
    (fn [] (gen/such-that #'vault-file/relative-file?
                          (gen/fmap #(java.io.File. %) (gen/string))))))

(defn- generate-one
  [spec]
  (first (gen/sample (s/gen spec) 1)))

(s/fdef id :ret ::id/t)
(defn id []
  (generate-one ::id/t))

(s/fdef vault-file :ret ::vault-file/t)
(defn vault-file []
  (generate-one ::vault-file/t))
