;;;; Wrappers for frequently used math functions requiring host interop
(ns hexwrench.math-interop)

(defn sqrt [x]
  #?(:clj (Math/sqrt x)
     :cljs (.sqrt js/Math x)))

(defn to-radians [x]
  #?(:clj (Math/toRadians x) 
     :cljs (* x (/ (.-PI js/Math) 180))))

(defn sin [x]
  #?(:clj (Math/sin x)
     :cljs (.sin js/Math x)))

(defn cos [x]
  #?(:clj (Math/cos x)
     :cljs (.cos js/Math x)))
