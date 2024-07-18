(ns hypertext-db.event-monitor
  "This namespace implements a file-system watcher for shared graphs.

  All it's public functions operate on graphs wrapped into atoms due to it's
  asynchronous nature.
  thread to a shared
  graph"
  (:require [clojure.spec.alpha                    :as s]
            [hawk.core                             :as hawk]
            [hypertext-db.event-monitor.hawk-specs :as hawk.specs]
            [hypertext-db.graph                    :as graph]
            [hypertext-db.utils.specs              :as u.specs]
            [hypertext-db.vault                    :as vault])
  (:import (clojure.lang Atom)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::hawk-watcher ::hawk.specs/watch)
(s/def ::t (s/merge ::graph/t
                    (s/keys :req [::hawk-watcher])))

(s/def ::context- (u.specs/atom* ::t))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Invariants and postconditions                                          ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defn- invariant-returns-argument
  "Returns a predicate which checks that the return value equals an argument."
  [get-argument-fn]
  #(= (:ret %) (get-argument-fn (:args %))))

(defn- invariant-allowed-to-modify
  "Invariant for a function which is only allowed to update some map keys."
  [get-arg-fn ks]
  #(= (apply dissoc (:ret %) ks)
      (apply dissoc (-> % :args get-arg-fn) ks)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Basic observers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef get-watcher
  :args (s/cat :x (s/or :wrapped (u.specs/atom* ::graph/t) :unwrapped ::graph/t))
  :ret  (s/nilable ::hawk-watcher))
(defmulti ^:private get-watcher #(if (instance? Atom %) :wrapped :unwrapped))
(defmethod get-watcher :wrapped   [a] (get-watcher @a))
(defmethod get-watcher :unwrapped [g] (::hawk-watcher g))

(s/fdef stopped?
  :args (s/cat :x (s/or :wrapped (u.specs/atom* ::graph/t) :unwrapped ::graph/t))
  :ret  boolean?)
(defn- stopped? [x] (nil? (get-watcher x)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Internal helpers                                                       ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef handle-event
  :args (s/cat :graph-atom ::context- :event ::hawk.specs/event))
(defn- handle-event
  [graph-atom event]
  (let [event-kind (:kind event)
        full-path  (:file event)]
    (case event-kind
      :create (swap! graph-atom graph/upsert-node-given-full-path- full-path)
      :modify (swap! graph-atom graph/upsert-node-given-full-path- full-path)
      :delete (swap! graph-atom graph/remove-node-given-full-path- full-path))
    graph-atom))

(s/fdef start
  :args (s/and (s/cat :graph ::graph/t :graph-atom (u.specs/atom* ::graph/t)))
  :ret  ::t
  :fn   (s/and (invariant-allowed-to-modify :graph [::hawk-watcher])))
(defn- start
  "Starts a file-system watcher and associates it to the graph."
  [graph graph-atom]
  (if (stopped? graph)
    (assoc graph ::hawk-watcher
           (hawk/watch! [{:paths [(::vault/dir graph)]
                          :context (constantly graph-atom)
                          :handler handle-event}]))
    graph))

(s/fdef stop
  :args (s/cat :graph ::graph/t)
  :ret  ::graph/t
  :fn   (s/and (invariant-allowed-to-modify :graph [::hawk-watcher])))
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
  :args (s/cat :graph-atom (u.specs/atom* ::graph/t))
  :ret  (u.specs/atom* ::t)
  :fn   (s/and (invariant-returns-argument :graph-atom)
               #(not (stopped? (:ret %)))))
(defn start! [graph-atom]
  (swap! graph-atom start graph-atom)
  graph-atom)

(s/fdef stop!
  :args (s/cat :graph-atom (u.specs/atom* ::graph/t))
  :ret (u.specs/atom* ::graph/t)
  :fn  (s/and (invariant-returns-argument :graph-atom)
              #(stopped? (:ret %))))
(defn stop!
  "Stops the watcher attached to the `graph-atom`."
  [graph-atom]
  (swap! graph-atom stop)
  graph-atom)
