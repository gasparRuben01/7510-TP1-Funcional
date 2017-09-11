(ns preposition
  (:require [bool :refer :all]))

(defrecord Preposition [ name facts rule ]
 Bool
 (bool [x n-upla]
  (or (contains? facts n-upla )
      (if rule
        (bool rule n-upla)
        false))))
