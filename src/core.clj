(ns core
  (:require [logical-interpreter :refer :all]))

(defn -main []
   (print (evaluate-query "  fact2(c1, c2, c3).
fact2(c2, c3, c1)." "fact2   (c2,c3   ,c1)"))
 )
