(ns hexwrench.core
  (:require [hexwrench.math-interop :as m]))

(def inner-diameter (m/sqrt 3))
(def apothem (/ inner-diameter 2))

;; Vertex coords for flat-topped unit hex, ccw from top right
(def hex-coords [[ 1/2 apothem]
                 [-1/2 apothem]
                 [-1   0]
                 [-1/2 (- apothem)]
                 [ 1/2 (- apothem)]
                 [ 1   0]])
  
