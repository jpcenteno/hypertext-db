(ns hypertext-db.event-monitor
  "This namespace implements a file-system watcher for shared graphs.

  All it's public functions operate on graphs wrapped into atoms due to it's
  asynchronous nature.
  thread to a shared
  graph"
  (:require [clojure.spec.alpha :as s]
            [hawk.core          :as hawk]
            [hypertext-db.graph :as graph]
            [hypertext-db.vault :as vault])
  (:import (clojure.lang Atom)
           (java.lang Thread)
           (java.nio.file WatchService)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

;; This namespace uses the Hawk library internally. This is the 
(s/def ::thread #(instance? Thread %))
(s/def ::watcher #(instance? WatchService %))
(s/def ::hawk-watcher (s/keys :req-un [::thread ::watcher]))

;; NOTE that using this `spec` to write function contracts is not thread safe.
;; We should expect false positives due to race conditions between the phases
;; of:
;; 1. Argument predicate validation.
;; 2. Function evaluation.
;; 3. Return predicate validation.
;; 4. Postcondition and invariants (`:fn`) validation.
;;
;; Nonetheless, this will be useful for catching bugs during the test phase.
;;
;; Passing a validator during atom creation would be the safest way to prevent
;; invalid states, but it comes with the added cost of running a costly spec
;; validation after each time the graph is updated.
(s/def ::graph-atom (s/and #(instance? Atom %)
                           #(s/valid? ::graph/t (deref %))))

(s/def ::t (s/merge ::graph/t
                    (s/keys :req [::hawk-watcher])))

(s/def ::t-atom (s/and #(instance? Atom %) #(s/valid? ::t (deref %))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Invariants and postconditions                                          ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- invariant-returns-argument
  "Returns a predicate which checks that the return value equals an argument."
  [get-argument-fn]
  #(= (:ret %) (get-argument-fn (:args %))))

(defn- invariant-only-allowed-to-update-these-keys
  "Invariant for a function which is only allowed to update some map keys."
  [get-arg-fn ks]
  #(= (apply dissoc (:ret %) ks)
      (apply dissoc (-> % :args get-arg-fn) ks)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Basic observers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef get-watcher
  :args (s/cat :x (s/or :wrapped ::graph-atom :unwrapped ::graph/t))
  :ret  (s/nilable ::hawk-watcher))
(defmulti ^:private get-watcher #(if (instance? Atom %) :wrapped :unwrapped))
(defmethod get-watcher :wrapped   [a] (get-watcher @a))
(defmethod get-watcher :unwrapped [g] (::hawk-watcher g))

(s/fdef stopped?
  :args (s/cat :x (s/or :wrapped ::graph-atom :unwrapped ::graph/t))
  :ret  boolean?)
(defn- stopped? [x] (nil? (get-watcher x)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Internal helpers                                                       ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef start
  :args (s/and (s/cat :graph ::graph/t))
  :ret  ::t)
(defn- start
  "Starts a file-system watcher and associates it to the graph."
  [graph]
  (if (stopped? graph)
    (assoc graph ::hawk-watcher
           (hawk/watch! [{:paths [(::vault/dir graph)]
                          :handler (fn [ctx e]
                                     (println "event: " e)
                                     (println "context: " ctx)
                                     ctx)}]))
    graph))

(s/fdef stop
  :args (s/cat :graph ::graph/t)
  :ret  ::graph/t
  :fn   (s/and (invariant-only-allowed-to-update-these-keys :graph [::hawk-watcher])))
(defn- stop
  "Stops any watcher attached to the graph, then dissocs it from the graph."
  [graph]
  (if-let [hawk-watcher (::hawk-watcher graph)]
    (do (hawk/stop! hawk-watcher)
        (dissoc graph ::hawk-watcher))
    graph))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Constructors                                                           ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef start!
  :args (s/cat :graph-atom ::graph-atom)
  :ret  ::t-atom
  :fn   (s/and (invariant-returns-argument :graph-atom)
               #(not (stopped? (:ret %)))))
(defn start! [graph-atom]
  (swap! graph-atom start)
  graph-atom)

(s/fdef stop!
  :args (s/cat :graph-atom ::graph-atom)
  :ret ::graph-atom
  :fn  (s/and (invariant-returns-argument :graph-atom)
              #(stopped? (:ret %))))
(defn stop!
  "Stops the watcher attached to the `graph-atom`."
  [graph-atom]
  (swap! graph-atom stop)
  graph-atom)
