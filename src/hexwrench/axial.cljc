(ns hexwrench.axial
  (:require [hexwrench.math-interop :as m]))

(defn distance [[x1 y1] [x2 y2]]
  (let [dx (m/abs (- x1 x2))
        dy (m/abs (- y1 y2))
        dxy (m/abs (- dx dy))]
    (unsigned-bit-shift-right (+ dx dy dxy) 1)))

(defn move
  ([] [0 0])
  ([hex] hex)
  ([[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
  ([hex1 hex2 & more]
    (reduce move (move hex1 hex2) more)))

(def neighbors [[1 1] [1 0] [0 -1] [-1 -1] [-1 0] [0 1]])

(defn get-neighbors [hex]
  (map (partial move hex) neighbors))