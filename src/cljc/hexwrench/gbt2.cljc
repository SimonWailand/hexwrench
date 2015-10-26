;;;; A libary for manipulating order 2 Generalized Balanced Ternary values
;;;; These values are sequences of base 7 numbers that are addresses of hexagons
;;;; that tile the 2D cartesion plane. I've never been able to get my hands on
;;;; Laurie Gibson and Dean Lucas's 1982 paper on the subject.
;;;; Wei Zeng Kitto's thesis on "An isomorphism between Generalized Balanced Ternary
;;;; and the p-adic integers" is a great source of information. As is the last chapter
;;;; of G. Ritter's "Image Algebra" (he was W. Kitto's advisor).
;;;; I'm using flat topped hexagons with the balanced aggregate layout from the original paper as listed below. Although I'm pretty sure any orientation will work with minimal code changes.
;;;;    1
;;;; 5     3
;;;;    0  
;;;; 4     2
;;;;    6
(ns hexwrench.gbt2
  (:require [hexwrench.core :as hc]))

;; According to https://github.com/RhysU/descendu/
;; who might have seen the original paper:
;; "Gibson and Lucas hint at something nicer...
;;     "There is a very quick and general algorithm for the
;;      addition of base digits in any dimension"
;; ...but then give no further hints.  For now, the
;; carry table is explicitly encoded."
(def add-carry-lut [[0 0 0 0 0 0 0]
                    [0 1 0 3 0 1 0]
                    [0 0 2 2 0 0 6]
                    [0 3 2 3 0 0 0]
                    [0 0 0 0 4 5 4]
                    [0 1 0 0 5 5 0]
                    [0 0 6 0 4 0 6]])

(def first-aggregate-clockwise [1 3 2 6 4 5])
(def angles-clockwise [0 60 120 180 240 300])

(def skew (Math/atan hc/apothem))

;; Could also do this via String casting, repeated division, or logarithms
;; Clojure uses Longs internally. MAX_VALUE is 9223372036854775807 or
;; 7r22341010611245052052300. Most GBT2 values will be small and always >= 0
(defn len [x]
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

(defn +mod7 [x y]
  (mod (+ x y) 7))

(defn *mod7 [x y]
  (mod (* x y) 7))

(defn pow7 [x]
  (long (Math/pow 7 x)));Math/pow returns a double

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

;; I always feel like I've failed if I have to use loop/recur, but I can't come
;; up with a better way to do this. I'd reduce, but I need to know the length
;; of the remaining sequence each step.
(defn seq->int [seq]
  (loop [s seq
         i 0]
    (if (empty? s)
      i
      (recur (rest s)
             (+ i (* (first s) (pow7 (- (count s) 1))))))))

(defn inv 
  "Returns the inverse of an address,
  which is the address equidistant across the origin"
  [x]
  (seq->int
   (map #(if (zero? %) 0 (- 7 %)) (int->seq x))))

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

;; Holy moly this seems gross. In other languages I would do a lot of array
;; index shenaningans here. The idea for this came from Knuth and
;; http://lburja.blogspot.com/2010/07/toy-algorithms-with-numbers-in-clojure.html
;; TODO: add an addition function that takes an aggregate level and wraps at that level
(defn add
  "Adds GBT2 addresses which is translation by vector addition"
  ([] 0)
  ([x] x)
  ([x y] 
   (loop [x-rev (int->revseq x)
          y-rev (int->revseq y)
          carries ()
          sum ()]
     (if (and (empty? x-rev) (empty? y-rev) (empty? carries))
       (let [unpadded-sum (drop-while zero? sum)]
         (if (empty? unpadded-sum)
           0
           (seq->int unpadded-sum)))
       (let [work (filter #(not (nil? %)) (conj carries (first x-rev) (first y-rev)))
             [curr-sum next-carry] (sum-digits work)]
         (recur (rest x-rev)
                (rest y-rev)
                next-carry
                (conj sum curr-sum))))))
  ([z y & more]
   (reduce add (add z y) more)))

(defn sub
  "Subtracts GBT2 addresses. x - y = x + inverse of y
  which returns the vector from y to x"
  ([] 0)
  ([x] (inv x))
  ([x y] (add x (inv y)))
  ([x y & more]
   (reduce sub (sub x y) more)))

;; I'm doing this with partial sums, should I rewrite it to add in place
;; like Knuth shows in his classical algorithms chapter?
;; GBT2 multiplication is performed mod 7 and has no carries, yay!
(defn mul
  "Multiply GBT2 addresses which performs rotation"
  ([] 0)
  ([x] x)
  ([x y]
   (let [y-seq (int->seq y)]
     (loop [x-rev (int->revseq x)
            place-padding ()
            partial-sums ()]
       (if (empty? x-rev)
         (seq->int (apply add partial-sums))
         (let [curr-multiplier (first x-rev)]
           (recur (rest x-rev)
                  (conj place-padding 0)
                  (if (zero? curr-multiplier)
                    partial-sums
                    (conj partial-sums
                          (concat (map #(*mod7 % curr-multiplier) y-seq)
                                  place-padding)))))))))
  ([x y & more]
   (reduce mul (mul x y) more)))

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
;; (arctan (/ (sqrt 3) 2))
(defn shortest-path 
  "Returns a sequence of unit translations that define the shortest path from
  addr1 to addr2. The count of this sequence is the Manhattan Distance.
  Any ordering of the sequence is valid assuming no obstacles"
  ([x]
   (loop [curr-hex x
          path ()]
     (println curr-hex)
     (if (every? zero? curr-hex)
       path
       (let [move (inv (take 1 curr-hex))];Move is the inverse of the most significant digit
         (println move)
         (recur (add curr-hex move)
                (conj path move))))))
  ([hex1 hex2]
   ;Translate "from" by moving "to" to the origin
   (shortest-path (sub hex1 hex2))))

;; Just implemented for radius 1 so far
(defn neighbors
  "returns the neighboring hexes around a hex in a given radius"
  [x]
  (map #(add x %) first-aggregate-clockwise))

(defn create-aggregate
  "Creates a set of hex addresses for aggregate n (7^n hexes)"
  [n]
  (range (Math/pow 7 n)))
