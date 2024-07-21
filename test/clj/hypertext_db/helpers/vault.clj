(ns hypertext-db.helpers.vault
  (:require [clojure.spec.alpha              :as s]
            [clojure.spec.gen.alpha          :as gen]
            [hypertext-db.vault              :as vault]
            [hypertext-db.vault.vault-file   :as vault-file]
            [hypertext-db.helpers.vault-file :as h.vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Specs for parameters                                                   ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::vault-files    (s/* ::vault-file/t))
(s/def ::subdirectories (s/* ::vault-file/relative-path))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Utility functions                                                      ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef mkdirs
  :args (s/cat :vault ::vault/t :subdirectories ::subdirectories))
(defn mkdirs
  [vault & relative-paths]
  (doseq [x relative-paths]
    (.mkdirs (File. (::vault/dir vault) x))))

(defn- touch
  ([vault & vault-files]
   (doseq [x vault-files]
     (h.vault-file/ensure-exists x vault))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Generators                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef generate
  :args (s/cat :keys (s/? (s/keys :opt-un [::vault-files ::subdirectories])))
  :ret ::vault/t)
(defn generate
  "Generates a vault"
  ([]
   (first (gen/sample (s/gen ::vault/t))))
  ([{:keys [vault-files subdirectories]}]
   (let [vault (generate)]
     (apply mkdirs vault subdirectories)
     (apply touch vault vault-files)
     vault)))
