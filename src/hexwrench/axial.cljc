(ns hexwrench.axial
  (:require [hexwrench.math-interop :as m]))

(defn distance [x1 y1 x2 y2]
  (let [dx (m/abs (- x1 x2))
        dy (m/abs (- y1 y2))
        dxy (m/abs (- dx dy))]
    (/ (+ dx dy dxy) 2)))
