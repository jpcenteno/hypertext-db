(ns hypertext-db.graph.backlinks-impl
  "This namespace provides specs and functions for internal use only. Use the
  interface provided by `hypertext-db.graph` instead."
  (:require [clojure.set             :as set]
            [clojure.spec.alpha      :as s]
            [hypertext-db.graph.node       :as node]
            [hypertext-db.vault.vault-file :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef value-does-not-contain-key?
  :args (s/cat :tuple (s/tuple any? coll?))
  :ret  boolean?)

(defn- value-does-not-contain-key?
  [[k v]]
  (not (contains? v k)))

(s/def ::t (s/and (s/map-of vault-file/id-spec ::node/backlinks)
                  #(every? value-does-not-contain-key? %)
                  #(not-any? empty? (vals %))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Observers                                                              ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef links?
  :args (s/cat :backlinks ::t :from vault-file/id? :to vault-file/id?)
  :ret  boolean?)
(defn- links?
  [backlinks from to]
  (contains? (get backlinks to) from))

(defn- ->links-set
  "Returns a collection of `::link/t` from `backlinks`"
  [backlinks]
  (persistent!
   (reduce (fn [acc [to froms]]
             (reduce (fn [acc from] (conj! acc [from to]))
                     acc
                     froms))
           (transient #{})
           backlinks)))

(s/fdef add
  :args (s/cat :backlinks ::t :from vault-file/id? :to vault-file/id?)
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
                      from (-> % :args :node vault-file/id)
                      tos  (-> % :args :node ::node/links)]
                  (every? (fn [to] (links? ret from to)) tos))))

(defn add-from-node
  [backlinks node]
  (let [from (vault-file/id node)
        tos  (::node/links node)]
    (reduce (fn [backlinks to] (add backlinks from to))
            backlinks
            tos)))

(defn- link-op-invariant-fn
  [f]
  #(let [{:keys [args ret]}          %
         {:keys [backlinks from to]} args]
     (f backlinks from to ret)))

(defn- node-op-invariant-fn
  [f]
  #(let [ret       (:ret %)
         backlinks (-> :args :backlinks)
         node      (-> :args :node)]
     (f ret backlinks node)))

(s/fdef remove-link
  :args (s/cat :backlinks ::t :from vault-file/id? :to vault-file/id?)
  :ret  ::t
  :fn   (s/or :contained     (s/and
                              (link-op-invariant-fn (fn [backlinks from to _ret] (links? backlinks from to)))
                              (link-op-invariant-fn (fn [backlinks from to ret] (= backlinks (add ret from to)))))
              :not-contained (s/and
                              (link-op-invariant-fn (fn [backlinks from to _ret] (not (links? backlinks from to))))
                              (link-op-invariant-fn (fn [backlinks _from _to ret] (= ret backlinks))))))

(defn remove-link
  [bs id-from id-to]
  (let [set' (disj (get bs id-to) id-from)]
    (if (empty? set')
      (dissoc bs id-to)
      (assoc bs id-to set'))))

(s/fdef remove-from-node
  :args (s/cat :backlinks ::t :node ::node/t)
  :ret  ::t
  :fn   (node-op-invariant-fn
         (fn [ret backlinks node]
           (= (->links-set ret)
              (set/difference (->links-set backlinks) (-> node ::node/->links set))))))
(defn remove-from-node
  [backlinks node]
  (let [id-from   (vault-file/id node)
        reduce-fn (fn [backlinks id-to]
                    (let [set' (disj (get backlinks id-to) id-from)]
                      (if (empty? set')
                        (dissoc backlinks id-to)
                        (assoc backlinks id-to set'))))]
    (reduce reduce-fn backlinks (::node/links node))))
