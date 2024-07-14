(ns hypertext-db.helpers.vault-file
  "Helpers for dealing with vault files that don't belong in library code."
  (:require [clojure.spec.alpha            :as s]
            [clojure.spec.gen.alpha        :as gen]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

(s/fdef java-file
  :args (s/cat :vault-file ::vault-file/t :vault ::vault/t)
  :ret  #(instance? File %))
(defn java-file
  "Returns a [[java.io.File]] for the `vault-file` at the `vault`."
  [vault-file vault]
  (let [base-path (::vault/dir vault)
        rel-path  (::vault-file/id vault-file)]
    (File. base-path (str rel-path))))

(s/fdef ensure-exists
  :args (s/cat :vault-file ::vault-file/t :vault ::vault/t)
  :ret  ::vault-file/t
  :fn   #(.exists (java-file (-> % :args :vault-file)
                             (-> % :args :vault))))
(defn ensure-exists
  "Ensures that the vault file exists. Returns the argument's `vault-file`."
  [vault-file vault]
  (let [file (java-file vault-file vault)]
    (doto (.getParentFile file)
      (.mkdirs))
    (doto file
      (.createNewFile)
      (.setLastModified (::vault-file/last-modified-ms vault-file))))
  vault-file)

(s/fdef ensure-does-not-exist
  :args (s/cat :vault-file ::vault-file/t :vault ::vault/t)
  :ret  ::vault-file/t
  :fn   #(not (.exists (java-file (-> % :args :vault-file)
                                  (-> % :args :vault)))))
(defn ensure-does-not-exist
  [vault-file vault]
  (doto (java-file vault-file vault)
    .delete)
  vault-file)

;;;; Generators

(s/fdef generate-distinct
  :args (s/cat :n pos-int?)
  :ret  (s/coll-of ::vault-file/t :distinct true :kind vector?))
(defn generate-distinct
  "Generates `n` distinct vault files"
  [n]
  (first (gen/sample (gen/vector-distinct (s/gen ::vault-file/t)
                                          {:num-elements n})
                     1)))
