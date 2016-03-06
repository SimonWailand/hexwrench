;;;; Holding cell for failed experiments and old code.
(ns hexwrench.deprecated)

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