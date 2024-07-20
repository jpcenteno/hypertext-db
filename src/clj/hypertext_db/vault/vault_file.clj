(ns hypertext-db.vault.vault-file
  "This namespace provides a model to represent files contained within the
  vault that are known to the database.
  
  By design, it only provides a data structure and does not perform any file
  system operation, relying instead on the data provided as parameters."
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as str])
  (:import (java.nio.file Paths)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private helpers for the ::relative-path spec                           ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- str->path [s]
  (Paths/get "" (into-array [s])))

(defn- relative-path? [s]
  (-> s str->path .isAbsolute not))

(defn- normalized-path? [s]
  (let [p (str->path s)] (= p (.normalize p))))

(defn- within-base-dir? [s]
  (not (str/starts-with? s "../")))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::relative-path (s/and string? #(not= "" %) relative-path? normalized-path? within-base-dir?))
(s/def ::last-modified-ms (s/and int? #(<= 0 %)))

(defn id? [x] (s/valid? ::relative-path x))

(s/def ::t (s/keys :req [::relative-path ::last-modified-ms]))

(def id-spec ::relative-path)

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Basic observers                                                ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn id
  "Returns the unique identifier of the `vault-file`."
  [vault-file]
  (::relative-path vault-file))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Constructors                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

;; FIXME We don't need this constructor.
(s/fdef file->
  :args (s/alt :unary  (s/cat :relative-path ::relative-path)
               :binary (s/cat :relative-path ::relative-path :last-modified-ms ::last-modified-ms))
  :ret  ::t
  :fn   (s/and #(= (-> % :ret ::relative-path) (-> % :args second :relative-path))
               #(= (-> % :ret ::last-modified-ms) (-> % :args second :last-modified-ms (or 0)))))
(defn file->
  "Constructs a `::vault-file/t` representing a file in the vault directory.
  Uses `last-modified-ms` if provided falling back to 0.

  By design decision, this namespace does not perform file-system IO
  operations, pushing that concern to the upper layers."
  ([file]
   (file-> file 0))
  ([file last-modified-ms]
   {::relative-path file
    ::last-modified-ms last-modified-ms}))
