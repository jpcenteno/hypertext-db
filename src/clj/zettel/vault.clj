(ns zettel.vault
  (:require [clojure.spec.alpha :as s])
  (:import (java.io File)))

(s/def ::dir (s/and #(instance? File %)
                    #(.isDirectory %)))

(s/def ::t (s/keys :req [::dir]))

(s/fdef dir->
  :args (s/cat :dir ::dir)
  :ret ::t
  :fn (s/and #(= (-> % :ret ::dir) (-> % :args :dir))))
(defn dir->
  [dir]
  {::dir dir})

(comment
  (s/fdef get-file-list
    :args (s/cat :vault ::t))
  (defn get-file-list
    [vault]
    (.listFiles (::dir vault))))
