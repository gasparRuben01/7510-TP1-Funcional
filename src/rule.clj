(ns rule
  (:require [bool :refer :all] ))

;;reordena los elementos de un vector  segun el orden indicado en la sequence,
;;por ejemplo si sequence=[2 1 0] el primer elemento del verctor retornado sera el que ocupaba
;;la posicion 2 del original, su segundo elmento sera el mismo que el segundo del original y el 
;;ultimo elmento sera el mismo que tenia el vector original en la primera posicion. Se puede repetir elementos y
;;no es obligatorio que todos los elementos del vector original aparezcan en el devuelto, por ejemplo sequence=(2 2 5 3) es una 
;;entrada valida.
(defn reorder 
  [vector sequence]
  (map (fn [x] (get vector x)) sequence))

;;order debe ser una sequence de numeros
(defrecord Rule [ preposition order]
  Bool
  (bool
    [x n-upla]
    (bool preposition (reorder n-upla order))))

;;rules es una sequence de Rules
(defrecord AndRule [rules]
  Bool
  (bool
    ;;AndRule es cierto, si todas las reglas contenidas en su campo rules es verdar
    [x n-upla]
    ;;filtro la sequence de reglas y dejo solo las que dan falsas, si la sequence esta vacia entonces es verda
    (= (count (filter (fn [y] (not (bool y n-upla)))  rules)) 0 )))

    
  
