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
  (:require [hexwrench.math-interop :as m]))

;; According to https://github.com/RhysU/descendu/ who might have seen the original paper:
;; "Gibson and Lucas hint at something nicer...
;; "There is a very quick and general algorithm for the addition of base digits in any dimension"
;; ...but then give no further hints.  For now, the carry table is explicitly encoded."
(def add-lut [[0 1 2 3 4 5 6]
              [1 2 3 4 5 6 0]
              [2 3 4 5 6 0 1]
              [3 4 5 6 0 1 2]
              [4 5 6 0 1 2 3]
              [5 6 0 1 2 3 4]
              [6 0 1 2 3 4 5]])

;; Notes: carry digit is only used when the addends are <= 60 degrees from each other.
;; The carry digit is the most clockwise addend
(def add-carry-lut [[nil nil nil nil nil nil nil]
                    [nil 1   nil 3   nil 1   nil]
                    [nil nil 2   2   nil nil 6  ]
                    [nil 3   2   3   nil nil nil]
                    [nil nil nil nil 4   5   4  ]
                    [nil 1   nil nil 5   5   nil]
                    [nil nil 6   nil 4   nil 6  ]])

;; if 0 then 0, else 7 - x
;; Same as multiplying by 6?
(def inv-lut [0 6 5 4 3 2 1])

(def mult-lut [[0 0 0 0 0 0 0]
               [0 1 2 3 4 5 6]
               [0 2 4 6 1 3 5]
               [0 3 6 2 5 1 4]
               [0 4 1 5 2 6 3]
               [0 5 3 1 6 4 2]
               [0 6 5 4 3 2 1]])

(def first-aggregate-cw [[1] [3] [2] [6] [4] [5]])
(def first-aggregate-angles-ccw [nil 0 240 300 120 60 180])

;; Change this to base 7? Just up to what fits in long max value
(def pow7 [1
           7
           49
           343
           2041
           16807
           117649
           823543
           5764801
           40353607
           282475249
           1977326743
           13841287201
           96889010407
           678223072849 
           4747561509943
           33232930569601
           232630513987207
           1628413597910449
           11398895185373144
           79792266297612000
           558545864083284032
           3909821048582988288])

(defn +mod7 [x y]
  (get-in add-lut [x y]))

(defn *mod7 [x y]
  (get-in mult-lut [x y]))

(defn int->seq
  "Convert an integer to a sequence of base 7 digits."
  [x] (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6} (m/base7 x)))

(defn seq->int
  "Convert a sequence of base 7 digits to an integer."
  [s] (apply + (map * (reverse s) pow7)))

(defn inv
  "Invert a GBT value."
  [x] (map inv-lut x))

(defn- sum-digits
  "Sums a sequence of digits and returns a tuple
  created by reducing over addition mod seven and collecting
  the sequence of carry digits [sum (carries)]"
  [coll]
  (reduce
    (fn [[prior-sum carries] x]
      (vector
        (+mod7 prior-sum x)
        (if-let [carry (get-in add-carry-lut [prior-sum x])]
          (conj carries carry)
          carries)))
    (vector (first coll) ())
    (rest coll)))

;; Holy moly this seems gross. In other languages I would do a lot of array
;; index shenaningans here. The idea for this came from Knuth and
;; http://lburja.blogspot.com/2010/07/toy-algorithms-with-numbers-in-clojure.html
(defn add 
  "Add GBT values, spatially equivalent to vector addition."
  ([] '(0))
  ([x] x)
  ([x y]
   (loop [x-rev (reverse x)
          y-rev (reverse y)
          carries ()
          sum ()]
     (let [work (concat carries (take 1 x-rev) (take 1 y-rev))]
       (if (empty? work)
         (if (or (empty? sum) (every? zero? sum)) '(0) sum)
         (let [[current-sum next-carries] (sum-digits work)]
           (recur (rest x-rev)
                  (rest y-rev)
                  next-carries
                  (conj sum current-sum)))))))
  ([x y & more]
   (reduce add (add x y) more)))

(defn sub 
  "Difference between GBT values, i.e. add the first value to the inverse of the second."
  ([] '(0))
  ([x] (inv x))
  ([x y] (add x (inv y))))

;; No carries in GBT multiplication. There is probably a way to do this without partial sums.
(defn mul
  "Multiply GBT values, spacially this is rotation."
  ([] '(0))
  ([x] x)
  ([x y]
    (apply add
      (map-indexed
        (fn [i d]
          (concat (map (partial *mod7 d) y) (repeat i 0)))
        (reverse x))))
  ([x y & more]
    (reduce mul (mul x y) more)))

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
(defn shortest-path
  "Returns a sequence of unit 1 translations to transform one GBT value to another.
  The count of this collection is the Manhattan Distance."
 ([x]
  (loop [curr-hex x
         path []]
    (if (every? zero? curr-hex)
        path
        (let [move (inv (take 1 curr-hex))]; Move is the inverse of the most significant digit
          (recur (add curr-hex move)
                 (conj path move))))))
 ([x y]
  (shortest-path (sub x y)))); Translate "from" by moving "to" to the origin

;; TODO: Just implemented for radius 1 so far, Needs to handle raidus n
;; Return a GBT value's neighbors for radius n.
(defn neighbors
  #_([x] (map (partial add x) first-aggregate-cw))
  ([x] (neighbors x 1))
  ([x n]
    (reduce add x (repeat n [1]))))

(defn create-aggregate
  "Creates a set of hex addresses (as integers) for aggregate n (7^n hexes)"
  [n] (range (pow7 n)))

;; Hmmm...this works, but lots of floating point inaccuracy. Will it actually be noticeable?
;; Maybe someday do without sin/cos, possible?
(defn to-cartesian
  "Converts a GBT2 address to Cartesian coordinates [x y]. Based on unit sided hexes."
  [x]
  (loop [x (drop-while zero? x)
         coords [0 0]]
    (if (empty? x)
      coords
      (let [offset (dec (count x))
            skew (* offset 19.11)
            theta (+ skew (first-aggregate-angles-ccw (first x)))
            r (m/sqrt (* 3 (pow7 offset)))
            [cx cy] coords]
        (recur (drop-while zero? (rest x))
               (vector (-> theta m/to-radians m/sin (* r) - (+ cx))
                       (-> theta m/to-radians m/cos (* r) (+ cy))))))))