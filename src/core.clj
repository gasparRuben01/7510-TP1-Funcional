(ns core
  (:require [logical-interpreter :refer :all]))

(defn -main []
   (print (evaluate-query "fact2  (c1, c2, c3) .  fact(3,2,1)  .   " "fact(   c,    2   , 1   )"))
 )
