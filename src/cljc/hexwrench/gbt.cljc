;;;; Shtuff about GBT in various dimensions here
(ns hexwrench.gbt)

(defprotocol GBTOps
  "Generalized Balanced Ternary Operations"
  (len [this] "Return the number of digits in a GBT value, i.e. what aggregate it is in.")
  (inv [this] "Invert a GBT value")
  (add [this that] "Add GBT values, spatially equivalent to vector addition")
  (sub [this that] "Difference between GBT values, i.e. add the first value to the inverse of the second")
  (mul [this that] "Multiply GBT values, spatially this is rotation")
  (shortest-path [this that] "")
  (neighbors [this n] "")
  (gen-aggregate [n] ""))

;; namespaces with only a protocol def get optimized out by the clojurescript compiler.
;; create a meaningless def to avoid the problem. Test this?
#?(:cljs (def x true))
