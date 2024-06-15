(ns zettel.vault
  (:require [clojure.spec.alpha :as s]
            [failjure.core :as f]
            [zettel.vault.vault-file :as vault-file])
  (:import (java.io File)))

(s/def ::dir (s/and #(instance? File %)
                    #(.isDirectory %)))

(s/def ::t (s/keys :req [::dir]))

(s/fdef dir->
  :args (s/cat :dir ::dir)
  :ret ::t
  :fn (s/and #(= (-> % :ret ::dir) (-> % :args :dir))))
(defn dir->
  [dir]
  {::dir dir})

(s/fdef list-vault-files
  :args (s/cat :vault ::t)
  :ret  (s/coll-of ::vault-file/t))
(defn list-vault-files
  [vault]
  (set (->> vault ::dir (.listFiles) (map vault-file/file->) (filter f/ok?))))
