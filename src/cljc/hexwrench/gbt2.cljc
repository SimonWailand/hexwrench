;;;; A library for manipulating Generalized Balanced Ternary values in two dimensions
;;;; Balanced Ternary is a base 3 number system. Spatially a one dimensional number line
;;;; GBT2 is a base 7 number system. Spatially a hexagonal tiling of the two dimensional plane
;;;; GBT3 is a base 15 number system. Spatially a tiling of 3d space using truncated octahedrons
;;;; GBTn is base 2^(n + 1) - 1 number system.

;;;; References:
;;;; "The Art of Computer Programming Vol2 Seminumerical Algorithms" by Donald Knuth (for Balanced Ternary)
;;;; Laurie Gibson and Dean Lucas's 1982 paper defining GBT (I've never obtained this)
;;;; "An isomorphism between Generalized Balanced Ternary and the p-adic integers" by Wei Zeng Kitto
;;;; "Image Algebra" by G. Ritter (Kitto's advisor)

;;;; I'm using the balanced aggregate layout from the original paper:
;;;;    1
;;;; 5     3
;;;;    0  
;;;; 4     2
;;;;    6
(ns hexwrench.gbt2
  (:require [hexwrench.core :as hex]))

;; According to https://github.com/RhysU/descendu/ who might have seen the original paper:
;; "Gibson and Lucas hint at something nicer...
;; "There is a very quick and general algorithm for the addition of base digits in any dimension"
;; ...but then give no further hints.  For now, the carry table is explicitly encoded."

;; Notes: carry digit is only used when the addends are <= 60 degrees from each other.
;; The carry digit is the most clockwise addend
(def add-carry-lut [[0 0 0 0 0 0 0]
                    [0 1 0 3 0 1 0]
                    [0 0 2 2 0 0 6]
                    [0 3 2 3 0 0 0]
                    [0 0 0 0 4 5 4]
                    [0 1 0 0 5 5 0]
                    [0 0 6 0 4 0 6]])

(def first-aggregate-clockwise [1 3 2 6 4 5])
(def angles-clockwise {1 0 3 60 2 120 6 180 4 240 5 300})

;; These can probably all be replaced by LUTs. Would it be faster? Find out someday.
(defn +mod7 [x y]
  (mod (+ x y) 7))

(defn *mod7 [x y]
  (mod (* x y) 7))

(defn pow7 [x]
  (long (Math/pow 7 x))); Math/pow returns a double

(defprotocol GBTOps
  "Generalized Balanced Ternary Operations"
  (len [x] "Return the number of digits in a GBT value, i.e. what aggregate it is in.")
  (inv [x] "Invert a GBT value.")
  (add [x y] "Add GBT values, spatially equivalent to vector addition.")
  (sub [x y] "Difference between GBT values, i.e. add the first value to the inverse of the second.")
  (mul [x y] "Multiply GBT values, spatially this is rotation.")
  (shortest-path [x] [x y] "Returns a sequence of unit 1 translations to transform one GBT value to another. The count of this collection is the Manhattan Distance.")
  (neighbors [x n] "Return a GBT value's neighbors for radius n."))

(defn int->seq [x]
  (lazy-seq
   (let [n (len x)
         p (pow7 (- n 1))
         d (quot x p)]
     (cons d (if (= n 1)
               nil
               (int->seq (- x (* d p))))))))

(defn int->revseq [x]
  (lazy-seq
   (cons (mod x 7)
         (let [q (quot x 7)]
           (if (pos? q) 
             (int->revseq q)
             nil)))))

(defn seq->int [seq]
  (loop [s seq
         i 0]
    (if (empty? s)
      i
      (recur (rest s)
             (+ i (* (first s) (pow7 (- (count s) 1))))))))

(defn- sum-digits
  "Sums a sequence of digits and returns a tuple
  created by reducing over addition mod seven and collecting
  the sequence of carry digits [sum (carries)]"
  [coll]
  (reduce (fn [sum-n-carries x]
            (let [[prior-sum carries] sum-n-carries
                  carry (get-in add-carry-lut [prior-sum x])]
              [(+mod7 prior-sum x)
               (if (not (or (nil? carry) (zero? carry)))
                 (conj carries carry)
                 carries)]))
          [(first coll) ()]
          (rest coll)))

(extend-protocol GBTOps
  Long
  (len [x]
    (cond
     (< x 7) 1
     (< x 49) 2
     (< x 343) 3
     (< x 2041) 4
     (< x 16807) 5
     (< x 117649) 6
     (< x 823543) 7
     (< x 5764801) 8
     (< x 40353607) 9
     (< x 282475249) 10
     (< x 1977326743) 11
     (< x 13841287201) 12
     (< x 96889010407) 13
     (< x 678223072849) 14
     (< x 4747561509943) 15
     (< x 33232930569601) 16
     (< x 232630513987207) 17
     (< x 1628413597910449) 18
     (< x 11398895185373144) 19
     (< x 79792266297612000) 20
     (< x 558545864083284032) 21
     :else 22))
  (inv [x]
    (seq->int (inv (int->seq x))))
  (add [x y])
  (sub [x y]
    (add x (inv y)))
  (mul [x y])
  (shortest-path 
    ([x])
    ([x y]))
  (neighbors [x n])

  clojure.lang.IPersistentCollection
  (len [x]
    (count x))
  (inv [x]
    (map #(if (zero? %) 0 (- 7 %)) x))
  ;; Holy moly this seems gross. In other languages I would do a lot of array
  ;; index shenaningans here. The idea for this came from Knuth and
  ;; http://lburja.blogspot.com/2010/07/toy-algorithms-with-numbers-in-clojure.html
  ;; TODO: add an addition function that takes an aggregate level and wraps at that level
  (add [x y]
    (loop [x-rev (reverse x)
           y-rev (reverse y)
           carries ()
           sum ()]
      (if (and (empty? x-rev) (empty? y-rev) (empty? carries))
        (let [unpadded-sum (drop-while zero? sum)]
          (if (empty? unpadded-sum) (conj (empty x) 0) unpadded-sum))
        (let [work (filter #(not (nil? %)) (conj carries (first x-rev) (first y-rev)))
              [curr-sum next-carry] (sum-digits work)]
          (recur (rest x-rev)
                 (rest y-rev)
                 next-carry
                 (conj sum curr-sum))))))
  (sub [x y]
    (add x (inv y)))
  (mul [x y]
    (loop [x-rev (reverse x)
           place-padding ()
           partial-sums ()]
      (if (empty? x-rev)
        (apply add partial-sums)
        (let [curr-multiplier (first x-rev)]
          (recur (rest x-rev)
                 (conj place-padding 0)
                 (if (zero? curr-multiplier)
                   partial-sums
                   (conj partial-sums
                         (concat (map #(*mod7 % curr-multiplier) y) place-padding))))))))
  ;; TODO: this only works for the first two aggregates. After that the skew means that 
  ;; the returned path is too long. Unskewing each translation by rotating based on 
  ;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
  (shortest-path
    ([x]
     (loop [curr-hex x
            path ()]
       (if (every? zero? curr-hex)
         path
         (let [move (inv (take 1 curr-hex))]; Move is the inverse of the most significant digit
           (recur (add curr-hex move)
                  (conj path move))))))
    ([x y]
     (shortest-path (sub x y)))); Translate "from" by moving "to" to the origin
  ;; Just implemented for radius 1 so far
  (neighbors [x n]
    (map #(add x %) first-aggregate-clockwise)))

(defn create-aggregate
  "Creates a set of hex addresses for aggregate n (7^n hexes)"
  [n]
  (range (Math/pow 7 n)))
