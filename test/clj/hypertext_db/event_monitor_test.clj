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
  `(let [~name (atom ~graph-constructor)]
     (try
       (event-monitor/start! ~name)
       ~@body
       (finally (event-monitor/stop! ~name)))))

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
          (is (graph/contains-node? graph (::vault-file/id vault-file)))))))

  (testing "Updates a file"
    (with-event-monitor [graph-atom (fixtures/graph-with-nodes-that-exist-in-vault)]
      (let [initial-graph-state @graph-atom
            some-vault-file     (-> initial-graph-state ::graph/nodes vals first)
            some-vault-file'    (helpers.vault-file/generate-updated
                                 some-vault-file
                                 {:vault-to-write-to initial-graph-state})]
        (Thread/sleep 1000) ; FIXME implement a function that insists until result or timeout.
        (is (= (graph/node-count initial-graph-state)
               (graph/node-count @graph-atom)))
        (is (graph/contains-node? @graph-atom (::vault-file/id some-vault-file')))
        (is (= (::vault-file/last-modified-ms some-vault-file')
               (::vault-file/last-modified-ms (graph/get-node @graph-atom (::vault-file/id some-vault-file')))))))))

(deftest test-delete-files
  (testing "Removes a node after it's file is deleted from the vault"
    (with-event-monitor [graph-atom (fixtures/graph-with-nodes-that-exist-in-vault)]
      (-> @graph-atom
          ::graph/nodes
          vals
          first
          (helpers.vault-file/ensure-does-not-exist @graph-atom))
      (Thread/sleep 1000)
      (is (zero? (graph/node-count @graph-atom))))))
