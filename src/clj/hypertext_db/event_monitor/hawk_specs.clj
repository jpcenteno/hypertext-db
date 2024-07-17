(ns hypertext-db.event-monitor.hawk-specs
  "(Internal) Spec definitions for data structures used by [[hawk.core]]."
  (:require [clojure.spec.alpha :as s])
  (:import (java.io File)
           (java.lang Thread)
           (java.nio.file WatchService)))

(s/def ::kind #{:create :modify :delete})
(s/def ::file #(instance? File %))
(s/def ::event (s/keys :req-un [::kind ::file]))

(s/def ::thread #(instance? Thread %))
(s/def ::watcher #(instance? WatchService %))
(s/def ::watch (s/keys :req-un [::thread ::watcher]))
