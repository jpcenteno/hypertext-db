(ns zettel.vault
  (:require [clojure.spec.alpha :as s]
            [failjure.core :as f]
            [zettel.id :as id]
            [zettel.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Module constants                                                       ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def ^:private max-retries
  "Max amount of retries for random id generation"
  50)

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Type specs                                                     ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::dir (s/and #(instance? File %)
                    #(.isDirectory %)))

(s/def ::t (s/keys :req [::dir]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private: Helpers                                                       ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef exists?
  :args (s/cat :vault ::t :vault-file ::vault-file/t)
  :ret boolean?)
(defn- exists?
  "Returns true if the file exists in the file system."
  [vault vault-file]
  (.exists (vault-file/->file vault-file (::dir vault))))

(defn- random-file
  "Generates a `java.io.File` using a random `::id` without creating it in the
  file system."
  [vault extension]
  (let [id-str   (id/->str (id/random))
        filename (str id-str "." extension)]
    (File. (::dir vault) filename)))

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

(s/fdef list-vault-files
  :args (s/cat :vault ::t)
  :ret  (s/coll-of ::vault-file/t))
(defn list-vault-files
  [vault]
  (set (->> vault ::dir (.listFiles) (map vault-file/file->) (filter f/ok?))))

(s/fdef create-empty-file
  :args (s/cat :vault ::t :extension ::vault-file/ext)
  :ret  ::vault-file/t
  :fn   (s/and #(= (-> % :args :extension) (-> % :ret ::vault-file/ext))
               #(exists? (-> % :args :vault) (:ret %))
               #(pos-int? (-> % :ret ::vault-file/last-modified-ms))))
(defn create-empty-file
  [vault extension]
  (loop [retries-left max-retries]
    (when (zero? retries-left)
      (throw (RuntimeException.
              (format "Failed to generate a random filename after %d retries"
                      max-retries))))
    (let [file (random-file vault extension)]
      (if (.createNewFile file)
        (vault-file/file-> file)
        (recur (dec retries-left))))))
