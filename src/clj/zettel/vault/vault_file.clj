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

(s/fdef valid-extension?
  :args (s/cat :s string?)
  :ret boolean?)
(defn- valid-extension?
  [s]
  (some? (re-matches file-extension-regex s)))

(s/def ::id ::id/zettel.id)
(s/def ::ext (s/and string? valid-extension?))
(s/def ::last-modified-ms (s/and #(instance? Long %) #(<= 0 %)))

(s/def ::t (s/keys :req [::id ::ext ::last-modified-ms]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Private helpers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

; (s/fdef validate-ext
;   :args (s/cat :x any?)
;   ; :ret  (s/or :ok ::ext :error f/failed?)
;   )
(defn validate-ext [x]
  (f/assert-with #(s/valid? ::ext %) x "Invalid extension"))

(defn validate-spec
  [spec x]
  (if (s/valid? spec x)
    x
    (f/fail (s/explain-str spec x))))

(s/fdef filename-and-extension
  :args (s/cat :file file?)
  :ret  (s/or :ok   (s/cat :file-name id/id-string? :extension ::ext)
              :fail f/failed?))
(defn- filename-and-extension
  "Splits file into its file-name and extension. Discards directories."
  [file]
  (let [without-directory    (.getName file)
        [filename extension] (str/split without-directory #"\." 2)]
    (f/attempt-all [filename  (validate-spec id/id-string? filename)
                    extension (validate-spec ::ext         extension)]
      [filename extension])))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public: Constructors                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef file->
  :args (s/cat :file file?)
  ; :ret  (s/or :ok ::t :error f/failed?)
  )
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
                  id              (id/str-> base-name)
                  ext             (validate-ext ext)]
    {::id               id
     ::ext              ext
     ::last-modified-ms (.lastModified file)}))
