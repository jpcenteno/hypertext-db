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
            [hypertext-db.specs.atom               :as s.atom]
            [hypertext-db.specs.fn                 :as s.fn]
            [hypertext-db.vault                    :as vault])
  (:import (clojure.lang Atom)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::hawk-watcher ::hawk.specs/watch)
(s/def ::t (s/merge ::graph/t
                    (s/keys :req [::hawk-watcher])))

(s/def ::context- (s.atom/atom* ::t))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Basic observers                                                        ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef get-watcher
  :args (s/cat :x (s/or :wrapped (s.atom/atom* ::graph/t) :unwrapped ::graph/t))
  :ret  (s/nilable ::hawk-watcher))
(defmulti ^:private get-watcher #(if (instance? Atom %) :wrapped :unwrapped))
(defmethod get-watcher :wrapped   [a] (get-watcher @a))
(defmethod get-watcher :unwrapped [g] (::hawk-watcher g))

(s/fdef stopped?
  :args (s/cat :x (s/or :wrapped (s.atom/atom* ::graph/t) :unwrapped ::graph/t))
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
      :create (swap! graph-atom graph/upsert-node-from-full-path- full-path)
      :modify (swap! graph-atom graph/upsert-node-from-full-path- full-path)
      :delete (swap! graph-atom graph/remove-node-from-full-path- full-path))
    graph-atom))

(s/fdef start
  :args (s/and (s/cat :graph ::graph/t :graph-atom (s.atom/atom* ::graph/t)))
  :ret  ::t
  :fn   (s.fn/allowed-keys :graph [::hawk-watcher]))
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
  :fn   (s.fn/allowed-keys :graph [::hawk-watcher]))
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
  :args (s/cat :graph-atom (s.atom/atom* ::graph/t))
  :ret  (s/and (s.atom/atom* ::t) (complement stopped?))
  :fn   (s.fn/returns-argument :graph-atom))
(defn start! [graph-atom]
  (swap! graph-atom start graph-atom)
  graph-atom)

(s/fdef stop!
  :args (s/cat :graph-atom (s.atom/atom* ::graph/t))
  :ret (s/and (s.atom/atom* ::graph/t) stopped?)
  :fn  (s.fn/returns-argument :graph-atom))
(defn stop!
  "Stops the watcher attached to the `graph-atom`."
  [graph-atom]
  (swap! graph-atom stop)
  graph-atom)
