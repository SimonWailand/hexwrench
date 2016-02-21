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
  (:require [hexwrench.core :as hex]
            [hexwrench.math-interop :as m]))

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
(def add-carry-lut [[0 0 0 0 0 0 0]
                    [0 1 0 3 0 1 0]
                    [0 0 2 2 0 0 6]
                    [0 3 2 3 0 0 0]
                    [0 0 0 0 4 5 4]
                    [0 1 0 0 5 5 0]
                    [0 0 6 0 4 0 6]])

(def mult-lut [[0 0 0 0 0 0 0]
               [0 1 2 3 4 5 6]
               [0 2 4 6 1 3 5]
               [0 3 6 2 5 1 4]
               [0 4 1 5 2 6 3]
               [0 5 3 1 6 4 2]
               [0 6 5 4 3 2 1]])

(def first-aggregate-cw [1 3 2 6 4 5])
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

(defn +multiplepow7 
  "Adds a multiple of a power of 7 to another integer"
  [x y z] (+ x (* y (pow7 z))))

(defn len
  "Return the number of digits in a GBT value, i.e. what aggregate it is in."
  [x] (if (zero? x) 1 
          (count (take-while (partial >= x) pow7))))

;; Not sure how to do this as a lazy-seq (i.e. not building it backwards)
;; without passing the length, otherwise 0s get dropped except last
(defn int->seq
  ([x]
   (int->seq x (dec (len x))))
  ([x n]
   (lazy-seq
    (let [p (pow7 n)]
      (cons (quot x p)
            (if-not (zero? n)
              (int->seq (mod x p) (dec n))))))))

;; This is like 5 times faster than (reverse (int->seq x))
(defn int->revseq [x]
  (lazy-seq
   (cons (mod x 7)
         (let [q (quot x 7)]
           (if (pos? q) 
             (int->revseq q))))))

(defn seq->int [seq]
  (loop [s seq
         i 0]
    (if (empty? s) i
        (recur (rest s)
               (+multiplepow7 i (first s) (dec (count s)))))))

(defn- sum-digits
  "Sums a sequence of digits and returns a tuple
  created by reducing over addition mod seven and collecting
  the sequence of carry digits [sum (carries)]"
  [coll]
  (reduce (fn [sum-n-carries x]
            (let [[prior-sum carries] sum-n-carries
                  carry (get-in add-carry-lut [prior-sum x])]
              [(+mod7 prior-sum x)
               (if-not (or (nil? carry) (zero? carry))
                 (conj carries carry)
                 carries)]))
          [(first coll) ()]
          (rest coll)))

;; Same as multiplying by 6?
(defn inv
  "Invert a GBT value."
  [x] (seq->int (map #(if (zero? %) 0 (- 7 %)) (int->seq x))))

(defn inv-seq [x]
  (map #(if (zero? %) 0 (- 7 0)) x))

;; Holy moly this seems gross. In other languages I would do a lot of array
;; index shenaningans here. The idea for this came from Knuth and
;; http://lburja.blogspot.com/2010/07/toy-algorithms-with-numbers-in-clojure.html
(defn add 
  "Add GBT values, spatially equivalent to vector addition."
  ([] 0)
  ([x] x)
  ([x y]
   (loop [x-rev (int->revseq x)
          y-rev (int->revseq y)
          carries ()
          aggregate 1
          sum 0]
     (if (and (empty? x-rev)
              (empty? y-rev)
              (empty? carries))
       sum
       (let [work (concat carries (take 1 x-rev) (take 1 y-rev))
             [current-sum next-carries] (sum-digits work)]
         (recur (rest x-rev)
                (rest y-rev)
                next-carries
                (inc aggregate)
                (+multiplepow7 sum current-sum (dec aggregate)))))))
  ([x y & more]
   (reduce add (add x y) more)))

;; Doesn't return what I expected in the first aggregate, works after that
(defn add-wrap
  "Add two GBT addresses with a maximum aggregate. Crossing into a higher aggregate
  instead wraps around as if the tiles were on the surface of a torus."
  ([n] 0)
  ([n x] x); Should I just drop digits above n here?
  ([n x y]
   (loop [x-rev (int->revseq x)
          y-rev (int->revseq y)
          carries ()
          aggregate 1
          sum 0]
     (if (or (> aggregate n)
             (and (empty? x-rev)
                  (empty? y-rev)
                  (empty? carries)))
       sum
       (let [work (concat carries (take 1 x-rev) (take 1 y-rev))
             [current-sum next-carries] (sum-digits work)]
         (recur (rest x-rev)
                (rest y-rev)
                next-carries
                (inc aggregate)
                (+multiplepow7 sum current-sum (dec aggregate)))))))
  ([n x y & more]
   (reduce (partial add-wrap n) (add-wrap n x y) more)))

(defn add-seq
  "Add GBT addresses that are already in the form of sequences and return a sequence.
  Useful for certain intermediary steps during other operations."
  ([] '(0))
  ([x] x)
  ([x y]
   )
  ([x y & more]
   (reduce add-seq (add-seq x y) more)))

(defn add-seq-wrap
  ""
  ([] '(0))
  ([x] x)
  ([n x y]
   ())
  ([n x y & more]
   (reduce (partial add-seq-wrap n) (add-seq-wrap n x y) more)))

(defn sub 
  "Difference between GBT values, i.e. add the first value to the inverse of the second."
  ([] 0)
  ([x] (inv x))
  ([x y]) (add x (inv y)))

(defn sub-seq
  ([] '(0))
  ([x] (inv-seq x))
  ([x y] (add-seq x (inv-seq y)))
  ([x y & more]
   (reduce sub-seq (sub-seq x y) more)))

(defn mul-new
  "New Multiply"
  [x y]
  (let [l (max (len x) (len y))]
    #_(apply +)
    (map *
         (for [xrev (take l (concat (int->revseq x) (repeat 0)))
               yrev (take l (concat (int->revseq y) (repeat 0)))]
           (do (println (*mod7 xrev yrev))
               (*mod7 xrev yrev)))
         (cycle (take l pow7)))))

(defn mul 
  "Multiply GBT values, spatially this is rotation."
  ([] 0)
  ([x] x)
  ([x y]
   (let [y (int->seq y)]
     (loop [x-rev (int->revseq x)
            place-padding ()
            partial-sums ()]
       (if (empty? x-rev)
         (do (println partial-sums)
             (reduce add partial-sums))
         (let [curr-multiplier (first x-rev)]
           (recur (rest x-rev)
                  (conj place-padding 0)
                  (if (zero? curr-multiplier)
                    partial-sums
                    (conj partial-sums
                          (seq->int (concat (map #(*mod7 % curr-multiplier) y) place-padding)))))))))))

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
(defn shortest-path
  "Returns a sequence of unit 1 translations to transform one GBT value to another.
  The count of this collection is the Manhattan Distance."
 ([x]
  (loop [curr-hex (int->seq x)
         path []]
    (if (every? zero? curr-hex) path
        (let [move (inv (first curr-hex))]; Move is the inverse of the most significant digit
          (recur (add curr-hex move)
                 (conj path move))))))
 ([x y]
  (shortest-path (sub x y)))); Translate "from" by moving "to" to the origin

;; Just implemented for radius 1 so far
;; Return a GBT value's neighbors for radius n.
(defn neighbors
  ([x] (map (partial add x) first-aggregate-cw))
  ([x n]))

(defn create-aggregate
  "Creates a set of hex addresses for aggregate n (7^n hexes)"
  [n] (range (pow7 n)))

;; Hmmm...this works, but lots of floating point inaccuracy. Will it actually be noticeable?
;; Maybe someday do without sin/cos
(defn to-cartesian
  "Converts a GBT2 address to Cartesian coordinates [x y]. Based on unit sided hexes."
  [x]
  (loop [x (drop-while zero? (int->seq x))
         coords [0 0]]
    (if (empty? x)
      coords
      (let [offset (dec (count x))
            skew (* offset 19.11)
            theta (+ skew (first-aggregate-angles-ccw (first x)))
            radius (m/sqrt (* 3 (pow7 offset)))
            [curr-x curr-y] coords]
        (recur (drop-while zero? (rest x))
               [(-> theta m/to-radians m/sin (* radius) - (+ curr-x))
                (-> theta m/to-radians m/cos (* radius) (+ curr-y))])))))
