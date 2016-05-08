(ns hexwrench.gbt2.diagrams.svg
  (:require [clojure.string :as s]
            [hexwrench.math-interop :as m]
            [hexwrench.core :as h]
            [hexwrench.gbt2 :as gbt2]
            [hiccup.core :refer [html]]))

(def scale-factor 10)

(def coord-string
  (s/join " " (map #(s/join "," (map (partial * scale-factor) %)) h/hex-coords)))

(defn hex-tile [hex & opts]
  (let [[id alive] hex
        [x y] (gbt2/to-cartesian (gbt2/int->seq id))
        x (* x scale-factor)
        y (- (* y scale-factor))
        hex-classes (s/join " " ["hexagon" (if alive "active")])]
    [:g.tile {:id (str "h" id)
              :transform (str "translate(" x " " y ")")}
     [:polygon {:class hex-classes
                :points coord-string}]
     (if (opts :show-ids) [:text.label (m/base7 id)])]))

(defn create-grid
  "Takes a sequence of gbt2 addresses and makes a svg visualization of them."
  [xs & opts]
  (let [opts (merge
               {:show-ids true
                :show-aggregates false
                :arrows []}
               opts)]
    (html
      [:svg {:baseProfile         "full"
             :preserveAspectRatio "xMidYMid slice"
             :version             "1.1"
             :viewBox             "0 0 100 100"
             :xmlns               "http://www.w3.org/2000/svg"
             :xmlns:svg           "http://www.w3.org/2000/svg"
             :xmlns:xlink         "http://www.w3.org/1999/xlink"}
       [:g#coordSysTransform {:transform "translate(50 50)"}
        (for [h xs]
          (hex-tile h opts))]])))
