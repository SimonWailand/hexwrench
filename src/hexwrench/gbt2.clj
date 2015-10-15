;;;; A libary for manipulating order 2 Generalized Balanced Ternary values
;;;; These values are sequences of base 7 numbers that are addresses of hexagons
;;;; that tile the 2D cartesion plane. I've never been able to get my hands on
;;;; Laurie Gibson and Dean Lucas's 1982 paper on the subject.
;;;; Wei Zeng Kitto's thesis on "An isomorphism between Generalized Balanced Ternary
;;;; and the p-adic integers" is a great source of information. As is the last chapter
;;;; of G. Ritter's "Image Algebra" (he was her advisor).
;;;; I'm using flat topped hexagons with the balanced aggregate layout from the original paper.
;;;;    1
;;;; 5     3
;;;;    0  
;;;; 4     2
;;;;    6
(ns hexwrench.gbt2)

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

(def first-aggregate-clockwise [[1] [3] [2] [6] [4] [5]])
(def angles-clockwise [0 60 120 180 240 300])

(defn gbt2? [hex]
  (every? #(<= 0 % 6) hex))

;; TODO: rewrite this so it works in both clojure and clojurescript?
(defn str->hex [s]
  (map #(Character/getNumericValue %) (seq s)))

(defn int->hex [x]
  (str->hex (Integer/toString x 7)))

(defn hex->str [hex]
  (apply str hex))

(defn hex->int [hex]
  (Integer/parseInt (hex->str hex)))

(defn inv 
  "Returns the inverse of an address,
  which is the address equidistant across the origin"
  [hex]
  (map #(if (zero? %) 0 (- 7 %)) hex))

(defn +mod7 [x y]
  (mod (+ x y) 7))

(defn *mod7 [x y]
  (mod (* x y) 7))

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
  ([] '(0))
  ([hex] hex)
  ([hex1 hex2] 
   (loop [hex1-rev (reverse hex1)
          hex2-rev (reverse hex2)
          carries ()
          sum-hex ()]
     (if (and (empty? hex1-rev) (empty? hex2-rev) (empty? carries))
       (let [unpadded-hex (drop-while zero? sum-hex)]
         (if (empty? unpadded-hex)
           '(0)
           unpadded-hex))
       (let [work (filter #(not (nil? %)) (conj carries (first hex1-rev) (first hex2-rev)))
             [sum next-carry] (sum-digits work)]
         (recur (rest hex1-rev)
                (rest hex2-rev)
                next-carry
                (conj sum-hex sum))))))
  ([hex1 hex2 & more]
   (reduce add (add hex1 hex2) more)))

(defn sub
  "Subtracts GBT2 addresses. x - y = x + inverse of y
  which returns the vector from y to x"
  ([] '(0))
  ([hex] (inv hex))
  ([hex1 hex2] (add hex1 (inv hex2)))
  ([hex1 hex2 & more]
   (reduce sub (sub hex1 hex2) more)))

;; I'm doing this with partial sums, should I rewrite it to add in place
;; like Knuth shows in his classical algorithms chapter?
;; GBT2 multiplication is performed mod 7 and has no carries, yay!
(defn mul
  "Multiply GBT2 addresses which performs rotation"
  ([] '(0))
  ([hex] hex)
  ([hex1 hex2]
   (loop [hex1-rev (reverse hex1)
          place-padding ()
          partial-sums ()]
     (if (empty? hex1-rev)
       (apply add partial-sums)
       (let [curr-multiplier (first hex1-rev)]
         (recur (rest hex1-rev)
                (conj place-padding 0)
                (if (zero? curr-multiplier)
                  partial-sums
                  (conj partial-sums
                        (concat (map #(*mod7 % curr-multiplier) hex2)
                         place-padding))))))))
  ([hex1 hex2 & more]
   (reduce mul (mul hex1 hex2) more)))

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. Skew is 19.11 degrees per aggregate
;; (arctan (/ (sqrt 3) 2))
(defn shortest-path 
  "Returns a sequence of unit translations that define the shortest path from
  addr1 to addr2. The count of this sequence is the Manhattan Distance.
  Any ordering of the sequence is valid assuming no obstacles"
  ([hex]
   (loop [curr-hex hex
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
  [hex]
  (map #(add hex %) first-aggregate-clockwise))

(defn create-aggregate
  "Creates a set of hex addresses for aggregate n (7^n hexes)"
  [n]
  (map int->hex (range (Math/pow 7 n))))
