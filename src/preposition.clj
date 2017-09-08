(ns preposition
  (:require [bool :refer :all]))

(defrecord Preposition [ name facts rule ]
 Bool
 (bool [x n-upla]
  (or (contains? facts n-upla ) (bool rule n-upla))))
