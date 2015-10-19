(ns hexwrench.axial)

(defn distance [x1 y1 x2 y2]
  (let [dx (Math/abs (- x1 x2))
        dy (Math/abs (- y1 y2))
        dxy (Math/abs (- dx dy))]
    (/ (+ dx dy dxy) 2)))
