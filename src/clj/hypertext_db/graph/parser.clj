(ns hypertext-db.graph.parser
  "This namespace defines a structure and helpers to declare parsers that read
  files from the vault and convert them into graph nodes."
  (:require [clojure.spec.alpha            :as s]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::can-parse?-fn ifn?)
(s/def ::parse-fn      ifn?)
(s/def ::t             (s/keys :req [::can-parse?-fn ::parse-fn]))

(s/def ::parser-chain  (s/coll-of ::t :kind sequential?))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Implementation                                                         ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- default-parse-fn
  [vault-file _vault]
  (node/vault-file-> vault-file))

(s/fdef select-parser-fn*
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t)
  :ret (s/nilable ::parse-fn))
(defn- select-parser-fn*
  [parser-chain vault-file]
  (::parse-fn (first (filter #((::can-parse?-fn %) vault-file) parser-chain))))

(s/fdef select-parse-fn
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t)
  :ret ::parse-fn)
(defn- select-parse-fn
  [parser-chain vault-file]
  (or (select-parser-fn* parser-chain vault-file) default-parse-fn))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public API                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef parse
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t :vault ::vault/t)
  :ret  ::node/t)
(defn parse
  "Parses a `vault-file` selecting the first parser capable of handling it."
  [parser-chain vault-file vault]
  ((select-parse-fn parser-chain vault-file) vault-file vault))
