(ns hypertext-db.graph.parser
  (:require [clojure.spec.alpha            :as s]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.vault            :as vault]
            [hypertext-db.vault.vault-file :as vault-file]))

; (s/def ::can-parse?-fn (s/fspec :args (s/cat :vault-file ::vault-file/t) :ret boolean?))
; (s/def ::parse-fn      (s/fspec :args (s/cat :vault-file ::vault-file/t :vault ::vault/t) :ret ::node/t))
(s/def ::can-parse?-fn ifn?)
(s/def ::parse-fn      ifn?)
(s/def ::t             (s/keys :req [::can-parse?-fn ::parse-fn]))

(s/def ::parser-chain  (s/coll-of ::t :kind sequential?))

(defn- default-parse-fn
  [vault-file _vault]
  (node/vault-file-> vault-file))

(s/fdef get-parser-fn*
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t)
  :ret (s/nilable ::parse-fn))
(defn- get-parser-fn*
  [parser-chain vault-file]
  (::parse-fn (first (filter #((::can-parse?-fn %) vault-file) parser-chain))))

(s/fdef get-parser-fn
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t)
  :ret ::parse-fn)
(defn- get-parse-fn
  [parser-chain vault-file]
  (or (get-parser-fn* parser-chain vault-file) default-parse-fn))

(s/fdef parse
  :args (s/cat :parser-chain ::parser-chain :vault-file ::vault-file/t :vault ::vault/t)
  :ret  ::node/t)
(defn parse
  [parser-chain vault-file vault]
  ((get-parse-fn parser-chain vault-file) vault-file vault))
