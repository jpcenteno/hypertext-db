(ns zettel.graph.backlinks-impl
  "This namespace provides specs and functions for internal use only. Use the
  interface provided by `zettel.graph` instead."
  (:require [clojure.spec.alpha :as s]
            [zettel.graph.link       :as link]
            [zettel.graph.node       :as node]
            [zettel.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef value-does-not-contain-key?
  :args (s/cat :tuple (s/cat :k any? :v coll?))
  :ret  boolean?
  :fn   #(let [v (get-in % [:args :tuple :v])
               k (get-in % [:args :tuple :k])
               ret (:ret %)]
           (not= ret (contains? v k))))
(defn- value-does-not-contain-key?
  [[k v]]
  (not (contains? v k)))

(s/def ::t (s/and (s/map-of ::vault-file/id ::node/backlinks)
                  #(every? value-does-not-contain-key? %)
                  #(not-any? empty? (vals %))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Observers                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef links?
  :args (s/cat :backlinks ::t :from ::vault-file/id :to ::vault-file/id)
  :ret  boolean?)
(defn- links?
  [backlinks from to]
  (contains? (get backlinks to) from))

(defn- ->links
  "Returns a collection of `::link/t` from `backlinks`"
  [backlinks]
  (persistent!
   (reduce (fn [acc [to froms]]
             (reduce (fn [acc from] (conj! acc [from to]))
                     acc
                     froms))
           (transient [])
           backlinks)))

(s/fdef add
  :args (s/cat :backlinks ::t :from ::vault-file/id :to ::vault-file/id)
  :ret  ::t
  :fn   (s/and #(let [ret  (:ret %)
                      from (-> % :args :from)
                      to (-> % :args :to)]
                  (links? ret from to))))
(defn add
  "Adds a `::link/t` to `backlinks`"
  [backlinks from to]
  (update backlinks to #(set (conj % from))))

(s/fdef add-from-node
  :args (s/cat :backlinks ::t :node ::node/t)
  :ret  ::t
  :fn   (s/and #(let [ret  (:ret %)
                      from (-> % :args :node ::vault-file/id)
                      tos  (-> % :args :node ::node/links)]
                  (every? (fn [to] (links? ret from to)) tos))))

(defn add-from-node
  [backlinks node]
  (let [from (::vault-file/id node)
        tos  (::node/links node)]
    (reduce (fn [backlinks to] (add backlinks from to))
            backlinks
            tos)))
