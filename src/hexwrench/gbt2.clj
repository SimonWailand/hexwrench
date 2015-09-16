(ns hexwrench.gbt2
  (:refer-clojure)
  (:require hexwrench.core))

;; Could this be better done just by (% (+ x y) 7)?
(def add-lut [[0 1 2 3 4 5 6]
              [1 2 3 4 5 6 0]
              [2 3 4 5 6 0 1]
              [3 4 5 6 0 1 2]
              [4 5 6 0 1 2 3]
              [5 6 0 1 2 3 4]
              [6 0 1 2 3 4 5]])

(def add-carry-lut [[0 0 0 0 0 0 0]
                    [0 1 0 3 0 1 0]
                    [0 0 2 2 0 0 6]
                    [0 3 2 3 0 0 0]
                    [0 0 0 0 4 5 4]
                    [0 1 0 0 5 5 0]
                    [0 0 6 0 4 0 6]])

;; (defn gbt2?
;;   [addr]
;;   ())

(defn inv
  [addr]
  (map #(if (zero? %) 0 (- 7 %)) addr))

;; (defn mul
;;   [addr1 addr2]
;;   (map #(mod (* ) 7) addr1))

(defn add
  ([] [0])
  ([addr] addr)
  ([addr1 addr2] ())
  ([addr1 addr2 & more]
   (reduce add (add addr1 addr2) more)))
