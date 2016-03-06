;;;; Replacement functions that take an aggregate level to clamp at.
;;;; Operations act as wrapping transformations as if on the surface of a torus.
;;;; Doesn't seem to act as I would have guessed in the first aggregate.
(ns hexwrench.gbt2.clamped
	(:require [hexwrench.core :as hex]
			      [hexwrench.math-interop :as m]
			      [hexwrench.gbt2 :exclude [add mul]]))

(defn add
  "Add two GBT addresses with a maximum aggregate. Crossing into a higher aggregate
  instead wraps around as if the tiles were on the surface of a torus."
  ([n] '(0))
  ([n x] '(x)); Should I just drop digits above n here?
  ([n x y]
   (loop [x-rev (reverse x)
          y-rev (reverse y)
          carries ()
          sum ()]
     (if (or (= (count sum) n)
             (and (empty? x-rev)
                  (empty? y-rev)
                  (empty? carries)))
       (if (empty? sum) '(0) sum)
       (let [[current-sum next-carries] (sum-digits (concat carries (take 1 x-rev) (take 1 y-rev)))]
         (recur (rest x-rev)
                (rest y-rev)
                next-carries
                (conj sum current-sum))))))
  ([n x y & more]
   (reduce (partial add n) (add n x y) more)))