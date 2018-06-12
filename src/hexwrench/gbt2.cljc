;;;; I'm using the balanced aggregate layout from the original paper:
;;;;    1
;;;; 5     3
;;;;    0
;;;; 4     2
;;;;    6
(ns hexwrench.gbt2
  (:require [hexwrench.math-interop :as m]
            [gbt.core :as gbt]))

(def gbt2-space (gbt/create-space 2))

;; Convenience partials
(def int->address (partial gbt/int->address gbt2-space))
(def address->string (partial gbt/address->string gbt2-space))
(def address->int (partial gbt/address->int gbt2-space))

(def inv (partial gbt/inv gbt2-space))
(def add (partial gbt/add gbt2-space))
(def sub (partial gbt/sub gbt2-space))
(def mul (partial gbt/mul gbt2-space))

(def first-aggregate-cw [[1] [3] [2] [6] [4] [5]])
(def first-aggregate-angles-ccw [nil 0 240 300 120 60 180])

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
(defn shortest-path
  "Returns a sequence of unit 1 translations to transform one GBT value to another.
  The count of this collection is the Manhattan Distance."
 ([x]
  (loop [curr-hex x
         path []]
    (if (= curr-hex [0])
        path
        (let [move (inv (vector (last curr-hex)))]; Move is the inverse of the most significant digit
          (recur (add curr-hex move)
                 (conj path move))))))
 ([x y]
  (shortest-path (sub x y)))); Translate "from" by moving "to" to the origin

(defn- add-repeatedly [x y] (iterate (partial add y) x))
(defn- all-sextants
  "Takes a set of GBT2 values returns their corresponding values in all sextants"
  [xs]
  (concat xs
          (mapcat (fn [x]
                    (map (partial mul x) xs))
                  (rest first-aggregate-cw))))

(defn neighbors
  "Returns the neighbors of a GBT2 value in radius n."
  ([x] (neighbors x 1))
  ([x n]
   (->>
     ;; Hexes in first sextant
     (add-repeatedly [1] [1])
     (take n)
     (mapcat
       (fn [m y] (take m (add-repeatedly y [4])))
       (range 1 (inc n)))
     ;; Add the other 5 sextants
     all-sextants
     ;; Translate from origin
     (map (partial add x)))))

(defn create-aggregate
  "Creates a set of hex addresses (as integers) for aggregate n (7^n hexes)"
  [n] (map int->address (range ((:pows gbt2-space) n))))

;; Hmmm...this works, but lots of floating point inaccuracy. Will it actually be noticeable?
;; TODO: There is definitely a way to do this with matrices. Look at Kitto, et al. 1994
(defn to-cartesian
  "Converts a GBT2 address vector to Cartesian coordinates [x y].
  Based on unit sided hexes. Multiply by actual side length to get final values."
  [address]
  (reduce-kv (fn [[x y :as coords] idx digit]
               (if (zero? digit)
                 coords
                 (let [skew (* idx 19.11)
                       theta (m/to-radians (+ skew (first-aggregate-angles-ccw digit)))
                       r (m/sqrt (* 3 ((:pows gbt2-space) idx)))]
                   [(-> theta m/sin (* r) - (+ x))
                    (-> theta m/cos (* r) (+ y))])))
             [0 0] address))
