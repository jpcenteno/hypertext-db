(ns hypertext-db.vault
  (:require [clojure.spec.alpha :as s]
            [pathetic.core :as path]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Type specs                                                     ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- file? [x]
  (instance? File x))

(s/def ::dir (s/and #(instance? File %)
                    #(.isDirectory %)))

(s/def ::t (s/keys :req [::dir]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Constructors                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef dir->
  :args (s/cat :dir ::dir)
  :ret ::t
  :fn (s/and #(= (-> % :ret ::dir) (-> % :args :dir))))
(defn dir->
  [dir]
  {::dir dir})

(s/fdef stat-file
  :args (s/cat :vault ::t :file file?)
  :ret ::vault-file/t)
(defn- stat-file
  [vault file]
  (let [relative-path    (path/relativize (::dir vault) file)
        last-modified-ms (.lastModified file)]
    (vault-file/file-> (File. relative-path)
                       last-modified-ms)))

(s/fdef list-vault-files
  :args (s/cat :vault ::t)
  :ret  (s/coll-of ::vault-file/t))
(defn list-vault-files
  [vault]
  (->> vault
       ::dir
       file-seq
       (filter #(.isFile %))
       (map (partial stat-file vault))
       set))

(s/fdef vault-file->java-file
  :args (s/cat :vault ::t :vault-file ::vault-file/t)
  :ret  file?)
(defn- vault-file->java-file
  [vault vault-file]
  (let [base (::dir vault)
        relative (-> vault-file ::vault-file/id str)]
    (File. base relative)))

(s/fdef slurp-vault-file
  :args (s/cat :vault ::t :vault-file ::vault-file/t)
  :ret string?)
(defn slurp-vault-file
  "Opens a `vault-file` and returns a string with it's contents."
  [vault vault-file]
  (slurp (vault-file->java-file vault vault-file)))
