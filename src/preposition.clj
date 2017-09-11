(ns preposition
  (:require [bool :refer :all]))

;;una preposition evaluarse como verdadera o falsa
;;en su campo facts tiene una collecion de hechos verdaderos
;;en su campo rule tiene otro objeto capas de evaluarse como verdadero o falso
(defrecord Preposition [ name facts rule ]
 Bool
 (bool [x n-upla]
  (or (contains? facts n-upla )
      (if rule
        (bool rule n-upla)
        false))))
