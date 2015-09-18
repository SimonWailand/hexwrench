(ns hexwrench.gbt2
  (:refer-clojure)
  (:require hexwrench.core))

(def add-carry-lut [[0 0 0 0 0 0 0]
                    [0 1 0 3 0 1 0]
                    [0 0 2 2 0 0 6]
                    [0 3 2 3 0 0 0]
                    [0 0 0 0 4 5 4]
                    [0 1 0 0 5 5 0]
                    [0 0 6 0 4 0 6]])

(defn gbt2-addr? [addr]
  (every? #(<= 0 % 6) addr))

(defn str->addr [s]
  (map #(Character/getNumericValue %) (vec s)))

(defn addr->str [addr]
  (apply str addr))

(defn inv
  [addr]
  (map #(if (zero? %) 0 (- 7 %)) addr))

(defn first-or-zero [addr]
  (if (empty? addr) 0 (first addr)))

(defn- sum-digits
  "Sums a sequence of digits and returns a tuple
  created by reducing the sums mod seven and collecting
  the sequence of carry digits [sum (carries)]"
  [coll]
  (reduce (fn [sc w]
            (let [sum (sc 0)
                  carries (sc 1)]
              [(mod (+ sum w) 7)
               (conj carries (get-in add-carry-lut [sum w]))]))
          [(first coll) ()]
          (rest coll)))

;; Holy moly this seems gross. In other languages I would do a lot of array
;; index shenaningans here. The idea for this came from Knuth and
;; http://lburja.blogspot.com/2010/07/toy-algorithms-with-numbers-in-clojure.html
(defn add
  ([] [0])
  ([addr] addr)
  ([addr1 addr2] 
   (loop [addr1-rev (reverse addr1)
          addr2-rev (reverse addr2)
          carry []
          sum-rev []]
     (if (and (empty? addr1-rev) (empty? addr2-rev) (empty? carry))
       (reverse sum-rev)
       (let [work (conj (conj carry (first addr1-rev)) (first addr2-rev))
             sum-n-carry (sum-digits work)
             sum (sum-n-carry 0)
             carry (sum-n-carry 1)]
         (recur (rest addr1-rev)
                (rest addr2-rev)
                sum
                (conj sum-rev carry))))))
  ([addr1 addr2 & more]
   (reduce add (add addr1 addr2) more)))

(defn sub
  ([] [0])
  ([addr] (inv addr))
  ([addr1 addr2] (add addr1 (inv addr2)))
  ([addr1 addr2 & more]
   (reduce sub (sub addr1 addr2) more)))

(defn mul
  ([] [0])
  ([addr] addr)
  ([addr1 addr2] ()); IMPLEMENT
  ([addr1 addr2 & more]
   (reduce mul (mul addr1 addr2) more)))
