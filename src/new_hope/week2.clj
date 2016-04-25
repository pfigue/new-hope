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

(defn n-pow
  "Returns a function to raise a number to the n power.
Example:

(def sq (n-pow 2))
(= (sq 3) 8)  ; 2^^3 == 8"
  [ n ]

  #(int (Math/pow % n)))

(facts "Simple closure example"
       ((n-pow 2) 16) => 256
       ((n-pow 8) 2)  => 256
       (map (n-pow 3) [1 2 3 4]) => [1 8 27 64]
       (map #((n-pow %) 2) [0 1 2 3 4]) => [1 2 4 8 16])


(defn cartesian-product-aux
  [ c c2 ]
  (loop [result nil cs c2]
    (if (empty? cs)
      result
      (recur (conj result [c (first cs)]) (rest cs)))))

(defn cartesian-product
  [ c1 c2 ]
  (loop [result #{} cs c1]
    (if (empty? cs)
      (into #{} result)
      (recur
       (concat result
               (cartesian-product-aux (first cs) c2))
       (rest cs)))))

(facts "Cartesian product"
       (cartesian-product #{1 2 3} #{4 5}) => #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}
       (count
        (cartesian-product (into #{} (range 10))
                           (into #{} (range 30)))) => 300)


(defn symmetric-difference
    [ c1 c2 ]
    (into #{} (concat
        (filter #(not (contains? c2 %)) c1)
        (filter #(not (contains? c1 %)) c2))))

(facts "Symmetric difference"
       (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) => #{2 4 6 7}
       (symmetric-difference #{:a :b :c} #{}) => #{:a :b :c}
       (symmetric-difference #{} #{4 5 6}) => #{4 5 6}
       (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) => #{[3 4] [1 2]}) 
