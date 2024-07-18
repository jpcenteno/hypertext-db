(ns hypertext-db.helpers.vault-file
  "Helpers for dealing with vault files that don't belong in library code."
  (:require [clojure.spec.alpha            :as s]
            [clojure.spec.gen.alpha        :as gen]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Internal type specs                                                    ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::write-to-this-vault (s/nilable ::vault/t))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Generators                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef updated-vault-file-generator
  :args (s/cat :vault-file ::vault-file/t))
(defn- updated-vault-file-generator
  "Returns a generator for updated versions of `vault-file`.

  ## Parameters

  - `vault-file` the vault file to generate an updated version of.

  ## Implementation details

  - It will only generate an updated value for [[::vault-file/last-modified-ms]]."
  [vault-file]
  (->> (s/gen ::vault-file/last-modified-ms)
       (gen/such-that #(not= (::vault-file/last-modified-ms vault-file) %))
       (gen/fmap #(assoc vault-file ::vault-file/last-modified-ms %))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Utility functions                                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef java-file
  :args (s/cat :vault-file ::vault-file/t :vault ::vault/t)
  :ret  #(instance? File %))
(defn java-file
  "Returns a [[java.io.File]] for the `vault-file` at the `vault`."
  [vault-file vault]
  (let [base-path (::vault/dir vault)
        rel-path  (vault-file/id vault-file)]
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
  :args (s/cat :n pos-int? :opts (s/? map?))
  :ret  (s/coll-of ::vault-file/t :distinct true :kind vector?))
(defn generate-distinct
  ([n]
   (first (gen/sample (gen/vector-distinct (s/gen ::vault-file/t)
                                           {:num-elements n})
                      1)))
  ([n {:keys [write-to-this-vault]}]
   (let [vault-files (generate-distinct n)]
     (when (some? write-to-this-vault)
       (doseq [vault-file vault-files]
         (ensure-exists vault-file write-to-this-vault))
       vault-files))))

(s/fdef generate-distinct-and-existing
  :args (s/cat :n pos-int? :vault ::vault/t)
  :ret  (s/coll-of ::vault-file/t :distinct true :kind vector?))
(defn generate-distinct-and-existing [n vault]
  (mapv #(ensure-exists % vault) (generate-distinct n)))

(s/fdef generate-updated-version
  :args (s/cat :vault-file ::vault-file/t
               :opts (s/keys :opt-un [::write-to-this-vault]))
  :ret  ::vault-file/t)
(defn generate-updated-version
  "Returns an updated version of `vault-file` optionaly updating it on the vault storage.

  ## Parameters

  - `vault-file`: A [[::vault-file/t]] to update.
  - `opts`: A map that accepts the following options:
    - `:write-to-this-vault`: Writes the updated version when passed a [[::vault/t]].

  ## Returns

  Returns an updated version of the [[::vault-file/t]] passed as argument."
  [vault-file {:keys [write-to-this-vault]}]
  (let [new-vault-file (-> (updated-vault-file-generator vault-file) (gen/sample 1) first)]
    (if (some? write-to-this-vault)
      (ensure-exists new-vault-file write-to-this-vault)
      new-vault-file)))
