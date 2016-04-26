# hexwrench

A Clojure library for manipulating hex-grids, with a focus on Generalized Balanced Ternary.

## Generalized Balanced Ternary

_There should be lots of great diagrams here as soon as the diagram generator is working._

A library for manipulating Generalized Balanced Ternary values in two dimensions.
Balanced Ternary is a base 3 number system where instead of using the digits 0,1,2 the digits -1,0,1.
Donald Knuth proclaims it the most beautiful number system. Spatially it can be represented as a one dimensional number line.

GBT2 is a base 7 number system. Spatially a hexagonal tiling of a two dimensional plane.
GBT3 is a base 15 number system. Spatially a tiling of 3d space using truncated octahedrons.
GBTn is base 2^(n + 1) - 1 number system.

### References:
"The Art of Computer Programming Vol2 Seminumerical Algorithms" by Donald Knuth (for Balanced Ternary)
Laurie Gibson and Dean Lucas's 1982 paper defining GBT (I've never obtained this)
"An isomorphism between Generalized Balanced Ternary and the p-adic integers" by Wei Zeng Kitto
"Image Algebra" by G. Ritter (Kitto's advisor)