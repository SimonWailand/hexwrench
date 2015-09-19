;;;; A libary for manipulating order 2 Generalized Balanced Ternary values
;;;; These values are sequences of base 7 numbers that are addresses of hexagons
;;;; that tile the 2D cartesion plane. I've never been able to get my hands on
;;;; Laurie Gibson and Dean Lucas's 1982 paper on the subject.
;;;; Wei Zeng Kitto's thesis on "An isomorphism between Generalized Balanced Ternary
;;;; and the p-adic integers" is a great source of information. As is the last chapter
;;;; of G. Ritter's "Image Algebra" (he was her advisor).
(ns hexwrench.gbt2
  (:refer-clojure))

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

(defn gbt2-addr? [addr]
  (every? #(<= 0 % 6) addr))

;; TODO: rewrite this so it works in both clojure and clojurescript?
(defn str->addr
  ([s] (map #(Character/getNumericValue %) (seq s)))
  ([s & xs] (map str->addr (conj xs s))))

(defn addr->str [addr]
  (apply str addr))

(defn inv 
  "Returns the inverse of an address,
  which is the address equidistant across the origin"
  [addr]
  (map #(if (zero? %) 0 (- 7 %)) addr))

(defn- sum-digits
  "Sums a sequence of digits and returns a tuple
  created by reducing over addition mod seven and collecting
  the sequence of carry digits [sum (carries)]"
  [coll]
  (reduce (fn [sum-n-carries x]
            (let [[prior-sum carries] sum-n-carries
                  carry (get-in add-carry-lut [prior-sum x])]
              [(mod (+ prior-sum x) 7)
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
  ([addr] addr)
  ([addr1 addr2] 
   (loop [addr1-rev (reverse addr1)
          addr2-rev (reverse addr2)
          carries ()
          sum-addr ()]
     (if (and (empty? addr1-rev) (empty? addr2-rev) (empty? carries))
       sum-addr
       (let [work (filter #(not (nil? %)) (conj carries (first addr1-rev) (first addr2-rev)))
             [sum next-carry] (sum-digits work)]
         (recur (rest addr1-rev)
                (rest addr2-rev)
                next-carry
                (conj sum-addr sum))))))
  ([addr1 addr2 & more]
   (reduce add (add addr1 addr2) more)))

(defn sub
  "Subtracts GBT2 addresses. x - y = x + inverse of y
  which returns the vector from y to x"
  ([] '(0))
  ([addr] (inv addr))
  ([addr1 addr2] (add addr1 (inv addr2)))
  ([addr1 addr2 & more]
   (reduce sub (sub addr1 addr2) more)))

;; I'm doing this with partial sums, should I rewrite it to add in place
;; like Knuth shows in his classical algorithms chapter? Otherwise, short-circuit zeroes?
;; GBT2 multiplication is performed mod 7 and has no carries, yay!
(defn mul
  "Multiply GBT2 addresses which performs rotation"
  ([] '(0))
  ([addr] addr)
  ([addr1 addr2]
   (loop [addr1-rev (reverse addr1)
          place-padding ()
          partial-sums ()]
     (println partial-sums)
     (if (empty? addr1-rev)
       (apply add partial-sums)
       (let [curr-multiplier (first addr1-rev)]
         (recur (rest addr1-rev)
                (conj place-padding 0)
                (if (zero? curr-multiplier)
                  partial-sums
                  (conj partial-sums
                        (concat
                         (map #(mod (* % curr-multiplier) 7) addr2)
                         place-padding))))))))
  ([addr1 addr2 & more]
   (reduce mul (mul addr1 addr2) more)))

;; TODO: this only works for the first two aggregates. After that the skew means that 
;; the returned path is too long. Unskewing each translation by rotating based on 
;; some threshold might be a solution. 
(defn shortest-path 
  "Returns a sequence of unit translations that define the shortest path from
  addr1 to addr2. The count of this sequence is the Manhattan Distance."
  [addr1 addr2]
  (let [from-addr ()]; Translate the "to" address to the origin
    ()))

(defn neighbors
  "returns the neighboring hexes around a hex in a given radius"
  [addr radius]
  ())
