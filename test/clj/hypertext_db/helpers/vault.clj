(ns hypertext-db.helpers.vault
  (:require [clojure.spec.alpha              :as s]
            [clojure.spec.gen.alpha          :as gen]
            [hypertext-db.vault              :as vault]
            [hypertext-db.vault.vault-file   :as vault-file]
            [hypertext-db.helpers.vault-file :as h.vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Utility functions                                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef make-subdirectory
  :args (s/cat :vault ::vault/t :relative-paths (s/* ::vault-file/relative-path)))
(defn make-subdirectory
  "Create a subdirectory at the vault directory."
  [vault & relative-paths]
  (doseq [x relative-paths]
    (.mkdirs (File. (::vault/dir vault) x))))

(defn touch
  ([vault & vault-files]
   (doseq [x vault-files]
     (h.vault-file/ensure-exists x vault))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Generators                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef generate
  :ret ::vault/t)

(defn generate
  "Generates a vault"
  ([]
   (first (gen/sample (s/gen ::vault/t))))
  ([& {:keys [vault-files subdirectories]}]
   (println "vault-files =" vault-files)
   (let [vault (generate)]
     (apply make-subdirectory vault subdirectories)
     (apply touch vault vault-files)
     vault)))
