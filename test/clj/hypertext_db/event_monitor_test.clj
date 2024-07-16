(ns hypertext-db.event-monitor-test
  (:require [clojure.test                    :refer [deftest is testing]]
            [hypertext-db.event-monitor      :as event-monitor]
            [hypertext-db.test.fixtures      :as fixtures]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Helpers                                                                ║
; ╚════════════════════════════════════════════════════════════════════════╝

(defmacro with-graph-atom [name & body]
  `(let [~name (atom (fixtures/graph-empty))]
     ~@body))

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
