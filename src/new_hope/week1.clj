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


(defn find-the-odd-numbers
  [ iterable ]

  (loop 
      [ acc [] l iterable ]
    (if (empty? l)
      acc
      (let [n (first l)]
        (recur
         (if (odd? n)
           (conj acc n)
           acc)
         (rest l))))))

(facts "Find the Odd Numbers"
       (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5))
       (= (find-the-odd-numbers [4 2 1 6]) '(1))
       (= (find-the-odd-numbers [2 2 4 6]) '())
       (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3)))


(defmulti diverse-reverse class)
(defmethod diverse-reverse String
  [ s ]
  (clojure.string/reverse s))
(defmethod diverse-reverse  :default
  [ s ]
  (reverse s))

(facts "A reverse function for all kind of types I know"
       (= (diverse-reverse "abc") "cba")
       (= (diverse-reverse [1 2 3]) [3 2 1]))


(defn palindrome?
  [ candidate ]
  (= candidate (diverse-reverse candidate)))

(facts "Palindrome Detector"
       (false? (palindrome? '(1 2 3 4 5)))
       (true? (palindrome? "racecar"))
       (false? (palindrome? [1 2 3 4 5]))
       (true? (palindrome? [1 2 3 2 1])))


(defn dup
  "Duplicates the elements of a sequence"
  [ s ]
  (loop [ result [] l s ]
    (if (empty? l)
      result
      (let
          [n (first l)]
        (recur
         (concat result [n n])
         (rest l))))))
 
(facts "Duplicate a Sequence"
       (= (dup [1 2 3]) '(1 1 2 2 3 3))
       (= (dup [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))


(defn compress-a-sequence
  "Removes duplicated elements in a sequence"
  [ s ]
  (loop [result []
         last-char nil
         l s]
    (if (empty? l)
      result
      (let [ current-char (first l) ]
        (if (= last-char current-char)
          (recur
           result last-char (rest l))
          (recur
           (concat result [current-char ])
           current-char (rest l)))))))

(facts "Compress a Sequence"
       (= (compress-a-sequence [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
       (= (compress-a-sequence [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))


(defn drop-nth
  "Drops n-th item of the sequence"
  [ s n ]

  (if (< n 1)
    (throw (Exception. "n-th item should be >= 1.")))

  (loop [result []
         position n
         origin s ]

    (if (empty? origin)
      result
      (if (= 1 position)
        (recur result n
               (rest origin))
        (recur
         (concat result [ (first origin) ])
         (- position 1)
         (rest origin))))))

(facts "Drop Every n-th Item"
       (drop-nth [1 2 3 4] 0) => (throws Exception #"n-th item should be >= 1.")
       (= (drop-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
       (= (drop-nth [:a :b :c :d :e :f] 2) [:a :c :e])
       (= (drop-nth [1 2 3 4 5 6] 4) [1 2 3 5 6]))
