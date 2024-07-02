(ns hypertext-db.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [pathetic.core           :as path]
            [hypertext-db.graph            :as graph]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file])
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

(s/fdef id :ret ::vault-file/id)
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

(defn vault-file-that-exists
  ([parent-directory]
   (vault-file-that-exists parent-directory {}))
  ([parent-directory attrs]
   (let [vf (vault-file attrs)
         file (File. parent-directory (str (::vault-file/id vf)))]
     (doto (File. (.getParent file))
       (.mkdirs))
     (doto file
       (.createNewFile)
       (.setLastModified (::vault-file/last-modified-ms vf)))
     vf)))

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
