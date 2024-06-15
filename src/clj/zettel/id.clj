(ns zettel.id
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [failjure.core :as f]))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Namespace constants                                                    ║
; ╚════════════════════════════════════════════════════════════════════════╝

(def ^:private ^:const n 4)

(def ^:private ^:const words
  (->> "./etc/bip39.txt" slurp str/split-lines (map keyword)))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Type specs                                                             ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/def ::word (set words))

(s/def ::zettel.id (s/coll-of ::word :into [] :count n))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public - Constructors                                                  ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef random
  :args (s/cat)
  :ret ::zettel.id)
(defn random
  []
  (vec (repeatedly n #(rand-nth words))))

(s/fdef str->
  :args (s/cat :s string?)
  :ret  (s/or :ok ::zettel.id :fail f/failed?))
(defn str->
  [s]
  (let [substrings (str/split s #"-")
        hopefuly-an-id (vec (map keyword substrings))]
    (if (s/valid? ::zettel.id hopefuly-an-id)
      hopefuly-an-id
      (f/fail (str "Failed to parse id string: " s)))))

(s/fdef id-string?
  :args (s/cat :x any?)
  :ret boolean?)
(defn id-string?
  [x]
  (and (string? x)
       (f/ok? (str-> x))))

; ╔════════════════════════════════════════════════════════════════════════╗
; ║ Public - Serialization                                                 ║
; ╚════════════════════════════════════════════════════════════════════════╝

(s/fdef ->str
  :args (s/cat :id ::zettel.id)
  :ret  id-string?
  :fn   (s/and #(f/ok? (:ret %))
               #(let [input  (-> % :args :id)
                      result (-> % :ret)
                      result-as-id (str-> result)]
                  (= result-as-id input))))
(defn ->str
  [id]
  (->> id (map name) (str/join "-")))