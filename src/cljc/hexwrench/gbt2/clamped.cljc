;;;; Replacement functions that take an aggregate level to clamp at.
;;;; Operations act as wrapping transformations as if on the surface of a torus.
;;;; Doesn't seem to act as I would have guessed in the first aggregate.
(ns hexwrench.gbt2.clamped)

(defn add-wrap
  "Add two GBT addresses with a maximum aggregate. Crossing into a higher aggregate
  instead wraps around as if the tiles were on the surface of a torus."
  ([n] 0)
  ([n x] x); Should I just drop digits above n here?
  ([n x y]
   (loop [x-rev (reverse (int->seq x))
          y-rev (reverse (int->seq y))
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

(defn add-seq-wrap
  ""
  ([] '(0))
  ([x] x)
  ([n x y]
   ())
  ([n x y & more]
   (reduce (partial add-seq-wrap n) (add-seq-wrap n x y) more)))