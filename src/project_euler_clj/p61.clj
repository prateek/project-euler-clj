(ns project-euler-clj.p61
  (:require [clojure.math.combinatorics :as combo]))

(defn polygon-number
  [k n] (case k
          3 (/ (* n (+ n 1)) 2)
          4 (* n n)
          5 (/ (* n (- (* n 3) 1)) 2)
          6 (* n (- (* n 2) 1))
          7 (/ (* n (- (* n 5) 3)) 2)
          8 (* n (- (* n 3) 2))
          nil))

(defn k-digit-number?
  [k] (let [k-minus-1   (- k 1)
            upper-bound (Math/pow 10 k)
            lower-bound (- (Math/pow 10 k-minus-1) 1)]
        (fn [n]
          (and
            (< n upper-bound)
            (> n lower-bound)))))

(defn four-digit-polygon-numbers
  [n] (filter
        (k-digit-number? 4)
        (map
          (partial polygon-number n) (range 150))))

; assumes 4 digit numbers
(defn- first-two-digits [x] (quot x 100))
(defn- last-two-digits [x] (rem x 100))
; check if last two digits of first number are
; the first two digits of second number
(defn- cycle-nums?  [x y] (= (last-two-digits x) (first-two-digits y)))

(def digits
  (into {}
        (for [i (range 3 9)]
          (let [nums (four-digit-polygon-numbers i)]
            {(symbol (str i))
             {:numbers   nums
              :first-two (group-by first-two-digits nums)
              :last-two  (group-by last-two-digits nums)
              }}))))

(defn find-cyclic-list
  ([]          (trampoline find-cyclic-list (range 3 9)))
  ([poly-perm] (trampoline find-cyclic-list
                           (map (fn [i] (:numbers ((symbol (str i)) digits))) poly-perm)
                           (fn [x] true)))
  ([poly-list item-selector]
   (let [fst (first poly-list)
         rst (rest poly-list)
         items (filter item-selector fst)]
     (if (empty? rst)
       items
       (for [i items
             j (find-cyclic-list
                 rst
                 (partial cycle-nums? i))]
         (cons i [j]))))
   ))

(reduce +
        (flatten
          (take 1 ;taking one because all the cyclic permutations are generated
                (filter
                  #(cycle-nums? (nth % 5) (nth % 0))
                  (reduce clojure.set/union
                          (map
                            (comp (partial map flatten) find-cyclic-list)
                            (combo/permutations (range 3 9))))))))

