(ns hypertext-db.graph.link
  (:require [clojure.spec.alpha :as s]
            [hypertext-db.vault.vault-file :as vault-file]))

(s/def ::t (s/and (s/cat :from ::vault-file/id :to ::vault-file/id)
                  #(not= (first %) (second %))))
