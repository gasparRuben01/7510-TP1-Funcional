(ns core
  (:require [logical-interpreter :refer :all]))

(defn -main []
   (print (evaluate-query "fact2  (c1, c2, c3). fact(X,Y,Z):-fact2(X,Y,Z).  " "fact(   1,    c2   , c3 )"))
 )
