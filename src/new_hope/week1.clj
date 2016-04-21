(ns new-hope.week1
  (:require [midje.sweet :refer :all]
            [criterium.core :as crit]))

;; After launching the REPL always run (refresh) at least once

;; `comment` always returns nil and does not evaluate its arguments
;; Might be handy to type expressions here and send them to REPL
;;  instead of typing them in REPL directly
(comment
  (- 10 (* 2 3))
  (.toUpperCase "hello world")
  (conj '(2 3 4) 1)
  (conj '(3 4) 2 1)
  )

;; For Week 1 days 1-3 just play around a bit, use comment form to write down expressions

;; Unit test is just an expression that declares expectations using a simple DSL
;; The whole form returns true or false when evaluated
(facts "some facts"
  (second [2 3 4]) => 3)

;; Day 4

(def second-to-last
  (fn [coll]
    (second (reverse coll))))

(comment
  (reverse [1 2 3 4])
  (second [4 3 2 1])
  (first (next [4 3 2 1]))
  (butlast [1 2 3 4])
  (last [1 2 3])
  )

(facts "about second-to-last"
  (second-to-last [1 2 3 4]) => 3
  (second-to-last [1]) => nil
  (second-to-last nil) => nil)

(comment
  ;; Simple time measurement (run once)
  (let [coll (vec (range 10000))]
    (time (second (reverse coll)))
    (time (last (butlast coll))))
  ;; More elaborate benchmark taking 10 seconds
  (let [coll (vec (range 10000))]
    (crit/quick-bench (second (reverse coll)))
    (crit/quick-bench (last (butlast coll))))
  )

;; Day n+1 of the rest of your Clojure life.
(defn sum-it-all-up
        [ iterable ]
        (loop [acc 0 l iterable]  ;; Tail Call Optimization
          (if (empty? l)
            acc
            (recur  ;; More TCO.
             (+ acc (first l))
             (rest l)))))

(facts "Sum It All Up"
       (= (sum-it-all-up [1 2 3]) 6)
       (= (sum-it-all-up (list 0 -2 5 5)) 8)
       (= (sum-it-all-up #{4 2 1}) 7)
       (= (sum-it-all-up '(0 0 -1)) -1)
       (= (sum-it-all-up '(1 10 3)) 14)
       (= (sum-it-all-up []) 0)
       (= (sum-it-all-up (list 5)) 5))

