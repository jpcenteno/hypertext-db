(ns hypertext-db.event-monitor-test
  (:require [clojure.test                    :refer [deftest is testing]]
            [hypertext-db.event-monitor      :as event-monitor]
            [hypertext-db.graph              :as graph]
            [hypertext-db.helpers.vault-file :as helpers.vault-file]
            [hypertext-db.test.fixtures      :as fixtures]
            [hypertext-db.vault.vault-file   :as vault-file]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Helpers                                                                ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defmacro with-graph-atom [name & body]
  `(let [~name (atom (fixtures/graph-empty))]
     ~@body))

(defmacro with-event-monitor [[name graph-constructor] & body]
  `(with-graph-atom ~name
     (let [~name (atom ~graph-constructor)]
       (try
         (event-monitor/start! ~name)
         ~@body
         (finally (event-monitor/stop! ~name))))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Tests                                                                  ║
; ╚════════════════════════════════════════════════════════════════════════╝

(deftest test-start
  (testing "Associates a running watcher handle to the graph"
    (is (with-graph-atom graph-atom-arg
          (let [graph-atom-ret (event-monitor/start! graph-atom-arg)]
            (is (not (#'event-monitor/stopped? graph-atom-ret)))))))

  (testing "Does nothing when provided an graph with a watcher"
    (is (with-graph-atom graph-atom-arg
          (let [watcher-arg (-> graph-atom-arg event-monitor/start! (#'event-monitor/get-watcher))
                watcher-ret (-> graph-atom-arg event-monitor/start! (#'event-monitor/get-watcher))]
            (is (= watcher-arg watcher-ret)))))))

(deftest test-stop

  (testing "Does nothing given a stopped graph atom"
    (is (let [graph-atom-arg (atom (fixtures/graph-empty))
              graph-atom-ret (event-monitor/stop! graph-atom-arg)]
          (is (= graph-atom-arg graph-atom-ret))
          (is (#'event-monitor/stopped? graph-atom-ret)))))

  (testing "Stops the watcher from a graph atom"
    (is (let [graph-atom-arg (event-monitor/start! (atom (fixtures/graph-empty)))
              graph-atom-ret (event-monitor/stop! graph-atom-arg)]
          (is (= graph-atom-arg graph-atom-ret) "Returns the same reference")
          (is (nil? (#'event-monitor/get-watcher graph-atom-ret)))
          (is (#'event-monitor/stopped? graph-atom-ret))))))

(deftest test-create-files
  (testing "Adds a file to the graph after its creation"
    (with-event-monitor [graph-atom (fixtures/graph-empty)]
      (let [vault-file (first (helpers.vault-file/generate-distinct 1))]
        (helpers.vault-file/ensure-exists vault-file @graph-atom)
        (Thread/sleep 1000)
        (let [graph @graph-atom]
          (is (= 1 (graph/node-count graph)))
          (is (graph/contains-node? graph (::vault-file/id vault-file))))))))

;; FIXME (deftest test-update-files)
;; FIXME (deftest test-delete-files)
