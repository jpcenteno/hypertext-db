(ns zettel.test.fixtures
  (:require [clojure.spec.alpha      :as s]
            [clojure.spec.gen.alpha  :as gen]
            [pathetic.core           :as path]
            [zettel.graph.node       :as node]
            [zettel.vault.vault-file :as vault-file])
  (:import (java.io File)))

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
  ([m] (let [node (merge (generate-one ::node/t) m)]
         ; Prevent the case where the random backlinks include an ID provided in
         ; `m`.
         (update node ::node/backlinks disj (node/id node)))))
