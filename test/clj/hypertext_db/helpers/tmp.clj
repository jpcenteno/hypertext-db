(ns hypertext-db.helpers.tmp
  (:require [clojure.java.io :as io]))

(def ^:dynamic dir :dir-not-initialized)

(defn- rm
  [file]
  (run! io/delete-file (reverse (file-seq file))))

(defn- random-tmp-dir-file []
  (io/file (System/getProperty "java.io.tmpdir")
           (str "hypertext-db-test-" (int (rand Integer/MAX_VALUE)))))

(defn- create-tmp-dir []
  (loop [attempts-left 10]
    (if (zero? attempts-left)
      (throw (Exception. "Failed to create tmp dir for the vault"))
      (let [dir (random-tmp-dir-file)]
        (if (.mkdir dir)
          dir
          (recur (dec attempts-left)))))))

(defmacro with-tmp-dir
  [body]
  `(binding [dir (#'create-tmp-dir)]
     (try (~@body)
          (finally (#'rm dir)))))
