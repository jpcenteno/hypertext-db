(ns zettel.vault.vault-file
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [failjure.core :as f]
            [zettel.id :as id])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Module constants                                                       ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def ^:private ^:const file-extension-regex #"^\w+(?:\.\w+)*$")

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- file? [x]
  (instance? java.io.File x))

(defn- dir? [x]
  (and (file? x) (.isDirectory x)))

(s/fdef valid-extension?
  :args (s/cat :s string?)
  :ret boolean?)
(defn- valid-extension?
  [s]
  (some? (re-matches file-extension-regex s)))

(s/def ::id ::id/t)
(s/def ::ext (s/and string? valid-extension?))
(s/def ::last-modified-ms (s/and int? #(<= 0 %)))

(s/def ::t (s/keys :req [::id ::ext ::last-modified-ms]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private helpers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef validate-file-extension
  :args (s/cat :x any?)
  :ret  (s/or :ok ::ext :error f/failed?))
(defn- validate-file-extension [x]
  (f/assert-with #(s/valid? ::ext %) x "Invalid file extension"))

(s/fdef validate-id-string
  :args (s/cat :x any?)
  :ret  (s/or :ok id/id-string? :fail f/failed?))
(defn- validate-id-string [x]
  (f/assert-with id/id-string? x "Invalid id string"))

(s/fdef filename-and-extension
  :args (s/cat :file file?)
  :ret  (s/or :ok   (s/cat :file-name id/id-string? :extension ::ext)
              :fail f/failed?))
(defn- filename-and-extension
  "Splits file into its file-name and extension. Discards directories."
  [file]
  (let [without-directory    (.getName file)
        [filename extension] (str/split without-directory #"\." 2)]
    (f/attempt-all [filename  (validate-id-string      filename)
                    extension (validate-file-extension extension)]
      [filename extension])))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Constructors                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef file->
  :args (s/cat :file file?)
  :ret  (s/or :ok ::t :error f/failed?))
(defn file->
  "Constructs a `::vault-file/t` given a file. Returns a Failjure `f/failed?`
  value when provided a file with an invalid name.
  
  THIS FUNCTION PERFORMS SIDE EFFECTS.

  DESIGN DECISION:

  The `vault-file` domain assumes that any file passed ito it resides at
  the top level of the `vault` directory. All directory related information
  will be discarded.
  
  ## Some philosophical notes
  
  Not every file conforms a valid `::vault-file/t`. Nonetheless, due to it's
  name, this function should accept any file instance."
  [file]
  (f/attempt-all [v               (filename-and-extension file)
                  [base-name ext] v
                  id              (id/str-> base-name)]
    {::id               id
     ::ext              ext
     ::last-modified-ms (.lastModified file)}))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Type conversions                                               ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef ->file
  :args (s/cat :vault-file ::t :parent-directory dir?)
  :ret  file?
  :fn   (s/and
         #(= (.getParentFile (:ret %)) (-> % :args :parent-directory))
         ;; This function uses `::id` and `::ext` to generate a filename. Here we check
         ;; that the generated filename can be used to retrieve those values.
         #(= (-> % :ret file-> ::id) (-> % :args :vault-file ::id))
         #(= (-> % :ret file-> ::ext) (-> % :args :vault-file ::ext))))
(defn ->file
  "Converts `vault-file` into a Java `java.io.File` instance."
  [vault-file parent-directory]
  (let [id-str    (id/->str (::id vault-file))
        extension (::ext vault-file)
        file-name (str id-str "." extension)]
    (File. parent-directory file-name)))
