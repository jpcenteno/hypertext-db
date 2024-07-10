(ns hypertext-db.test.simple-node-parser
  (:require [clojure.spec.alpha            :as s]
            [clojure.string                :as str]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.graph.parser     :as parser]
            [hypertext-db.test.fixtures    :as fixtures]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file])
  (:import (java.io File)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Implementation internals                                               ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def extension ^:private ".link-list")

(s/fdef can-parse?
  :args (s/cat :vault-file ::vault-file/t)
  :ret  boolean?)
(defn- can-parse?
  [vault-file]
  (-> vault-file ::vault-file/id (str/ends-with? extension)))

(s/fdef ->node
  :args (s/and (s/cat :vault-file ::vault-file/t :vault ::vault/t))
  :ret  ::node/t)
(defn- ->node
  [vault-file vault]
  (let [content (vault/slurp-vault-file vault vault-file)
        links   (->> content str/split-lines (map #(File. %)) set)]
    (-> vault-file node/vault-file-> (assoc ::node/links links))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public test helpers                                                    ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def parser
  {::parser/can-parse?-fn can-parse?
   ::parser/parse-fn     ->node})

(s/fdef create-vault-file
  :args (s/cat :vault ::vault/t :links ::node/links)
  :ret  ::vault-file/t)
(defn create-vault-file
  [vault links]
  (let [vault-file (update (fixtures/vault-file) ::vault-file/id (fn [f] (File.  (str f extension))))
        file       (#'vault/vault-file->java-file vault vault-file)
        content    (->> links (map str) (str/join "\n"))]
    (spit file content)
    vault-file))
