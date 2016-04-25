(ns new-hope.week2
  (:require [midje.sweet :refer :all]
            [criterium.core :as crit]))


(defn lazy-fibo
        "Lazy Fibonacci Sequence Generator. Not TCO.
Be careful, cause it is lazy.
Use like this:

(take 10 (lazy-fibo))"
        ([] (lazy-fibo 1 1))
        ([a b] (lazy-seq (cons a (lazy-fibo b (+ b a))))))

(defn return-fibo-nums
  "Return the n first Fibonacci numbers"
  [ n ]
  (take n (lazy-fibo)))

(facts "Return the n first Fibonacci numbers."
 (return-fibo-nums 3) => '(1 1 2)
 (return-fibo-nums 6) => '(1 1 2 3 5 8)
 (return-fibo-nums 8) => '(1 1 2 3 5 8 13 21))
