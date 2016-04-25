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


(defn get-the-caps
  "Return only the capital letters"
  [ i ]

  (loop [ result [] input i]
    (if (empty? input)
      (clojure.string/join result)
      (let [letter (first input)]
        (if (Character/isUpperCase letter)
          (recur
           (concat result [letter])
           (rest input))
          (recur result (rest input)))))))

(facts "Only capital letters"
       (get-the-caps "HeLlO, WoRlD!") => "HLOWRD"
       (get-the-caps "nothing") => ""
       (get-the-caps "$#A(*&987Zf") => "AZ")


(defn half-truth
  [ boolis ]
  (not (or
        (nil? (some true? boolis))
        (every? true? boolis))))

(facts "Some but not all of the parameters should be truth"
       (half-truth (list true true true)) => false
       (half-truth (list true false true)) => true
       (half-truth (list false false false)) => false)
