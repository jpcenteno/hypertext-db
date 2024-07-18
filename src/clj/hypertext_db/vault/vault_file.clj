(ns hypertext-db.vault.vault-file
  "This namespace provides a model to represent files contained within the
  vault that are known to the database.
  
  By design, it only provides a data structure and does not perform any file
  system operation, relying instead on the data provided as parameters."
  (:require [clojure.spec.alpha :as s]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- relative-file?
  "Is `x` a `java.io.File` instance containing a relative path?"
  [x]
  (and (instance? java.io.File x)
       (not (.isAbsolute x))
       (not= (java.io.File. "") x)))

(s/def ::id relative-file?)
(s/def ::last-modified-ms (s/and int? #(<= 0 %)))

(s/def ::t (s/keys :req [::id ::last-modified-ms]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Basic observers                                                ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn id
  "Returns the unique identifier of the `vault-file`."
  [vault-file]
  (::id vault-file))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Constructors                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef file->
  :args (s/alt :unary  (s/cat :file relative-file?)
               :binary (s/cat :file relative-file? :last-modified-ms ::last-modified-ms))
  :ret  ::t
  :fn   (s/and #(= (-> % :ret ::id) (-> % :args second :file))
               #(= (-> % :ret ::last-modified-ms) (-> % :args second :last-modified-ms (or 0)))))
(defn file->
  "Constructs a `::vault-file/t` representing a file in the vault directory.
  Uses `last-modified-ms` if provided falling back to 0.

  By design decision, this namespace does not perform file-system IO
  operations, pushing that concern to the upper layers."
  ([file]
   (file-> file 0))
  ([file last-modified-ms]
   {::id file
    ::last-modified-ms last-modified-ms}))
