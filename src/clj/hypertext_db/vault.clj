(ns hypertext-db.vault
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [pathetic.core :as path]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Type specs                                                     ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- file? [x]
  (instance? File x))

(s/fdef absolute-file?
  :args (s/cat :x any?)
  :ret  boolean?)
(defn absolute-file? [x]
  (and (file? x) (.isAbsolute x)))

(s/fdef absolute-file-in-vault?
  :args (s/cat :vault ::t :absolute-file absolute-file?)
  :ret  boolean?)
(defn absolute-file-in-vault?
  [vault absolute-file]
  (str/starts-with?
   (.getCanonicalPath absolute-file)
   (.getCanonicalPath (::dir vault))))

(s/def ::dir (s/and #(instance? File %)
                    #(.isDirectory %)))

(s/def ::t (s/keys :req [::dir]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ File path helpers                                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef absolute-file->relative-file
  :args (s/and (s/cat :vault ::t :absolute-file absolute-file?)
               #(absolute-file-in-vault? (:vault %) (:absolute-file %)))
  :ret  ::vault-file/id
  :fn   #(= (-> % :args :absolute-file)
            (File. (-> % :args :vault ::dir) (-> % :ret str))))
(defn- absolute-file->relative-file
  "Returns a file with it's path relative to the `vault` base directory."
  [vault absolute-file]
  (-> (::dir vault)
      .toPath
      (.relativize (.toPath absolute-file))
      .toFile))

(s/fdef absolute-file->vault-file
  :args (s/and (s/cat :vault ::t :absolute-file absolute-file?)
               #(absolute-file-in-vault? (:vault %) (:absolute-file %)))
  :ret  ::vault-file/t)
(defn absolute-file->vault-file
  "Casts an absolute file into a [[::vault-file/t]] relative to `vault`."
  [vault absolute-file]
  (vault-file/file-> (absolute-file->relative-file vault absolute-file)
                     (.lastModified absolute-file)))

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

(s/fdef slurp-vault-file
  :args (s/cat :vault ::t :vault-file ::vault-file/t)
  :ret string?)
(defn slurp-vault-file
  "Opens a `vault-file` and returns a string with it's contents."
  [vault vault-file]
  (let [base (::dir vault)
        relative (-> vault-file ::vault-file/id str)]
    (slurp (File. base relative))))
