(ns hypertext-db.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [pathetic.core           :as path]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file]
            [hypertext-db.graph :as graph])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private helpers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- create-a-temporary-directory
  "Creates a temporary directory that will be deleted when the java VM running
  the tests terminates.

  This is necessary for specs that require existing directories."
  []
  (let [prefix "hypertext-db-test-"
        attrs  (into-array java.nio.file.attribute.FileAttribute [])
        path   (java.nio.file.Files/createTempDirectory prefix attrs)
        file   (.toFile path)]
    (doto file
      .deleteOnExit)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Spec re-definitions with added generators                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::vault/dir
  (s/with-gen ::vault/dir
    #(gen/fmap (fn [_] (create-a-temporary-directory))
               (gen/return 0))))

(def ^:private relative-file-generator
  (->> (s/gen (s/+ (s/and string? #(not= "" %)))) ; -> Path stems
       (gen/fmap path/render-path) ; -> Path string
       (gen/fmap #(File. %)))) ; -> File

(s/def ::vault-file/id
  (s/with-gen ::vault-file/id
    (fn [] relative-file-generator)))

(defn- generate-one
  [spec]
  (first (gen/sample (s/gen spec) 1)))

;;;; hypertext-db.vault.vault-file

(s/fdef vault-and-vault-file->java-file
  :args (s/cat :vault ::vault/t :vault-file ::vault-file/t)
  :ret  #(instance? File %))
(defn- vault-and-vault-file->java-file
  [vault vault-file]
  (let [base-path (::vault/dir vault)
        rel-path  (::vault-file/id vault-file)]
    (File. base-path (str rel-path))))

(defn id
  ([]  (generate-one ::vault-file/id))
  ([s] (File. s)))

(s/fdef vault-file :ret ::vault-file/t)
(defn vault-file
  ([]
   (vault-file {}))
  ([attrs]
   (merge (generate-one ::vault-file/t)
          attrs)))

(s/fdef vault-file-that-exists
  :args (s/or :unary (s/cat :vault ::vault/t)
              :binary (s/cat :vault ::vault/t :attrs map?))
  :ret ::vault-file/t
  :fn  #(let [vault      (-> % :args second :vault)
              vault-file (-> % :ret)
              file       (vault-and-vault-file->java-file vault vault-file)]
          (.exists file)))
(defn vault-file-that-exists
  ([vault]
   (vault-file-that-exists vault {}))
  ([vault attrs]
   (let [vf (vault-file attrs)
         file (vault-and-vault-file->java-file vault vf)]
     (doto (.getParentFile file)
       (.mkdirs))
     (doto file
       (.createNewFile)
       (.setLastModified (::vault-file/last-modified-ms vf)))
     vf)))

(defn vault-file-delete
  [vault vault-file]
  (.delete (vault-and-vault-file->java-file vault vault-file)))

;;;; hypertext-db.graph.node

(s/fdef node :ret ::node/t)
(defn node
  "Generates a random `::node/t`."
  ([]  (node {}))
  ([m] (let [node (merge (generate-one ::node/t) m)
             id   (::vault-file/id node)]
         ; Prevent the case where the random backlinks include an ID provided in
         ; `m`.
         (-> node
             (update ::node/links     disj id)
             (update ::node/backlinks disj id)))))

;;;; hypertext-db.vault

(s/fdef vault :ret ::vault/t)
(defn vault
  "Generates a vault with an existing temporary directory for testing purposes."
  []
  (generate-one ::vault/t))

;;;; hypertext-db.graph

(s/fdef graph-empty
  :args (s/cat)
  :ret ::graph/t
  :fn  #(zero? (graph/node-count (:ret %))))
(defn graph-empty []
  (graph/vault-> (vault)))

(defn graph-with-nodes-that-exist-in-vault
  "Returns a [[hypertext-db.graph]] which contains nodes that exist in its vault."
  []
  (let [graph (graph-empty)]
    (vault-file-that-exists graph)
    (graph/batch-sync-graph-with-vault graph)))
