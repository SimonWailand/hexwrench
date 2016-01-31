(ns hexwrench.core)

(def inner-diameter (Math/sqrt 3))
(def apothem (/ inner-diameter 2))

;; Vertex coords for flat-topped unit hex, ccw from top right
(def hex-coords [[0.5 apothem]
                 [-0.5 apothem]
                 [-1 0]
                 [-0.5 (- apothem)]
                 [0.5 (- apothem)]
                 [1 0]])
  
