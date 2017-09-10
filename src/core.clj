(ns core
  (:require [logical-interpreter :refer :all]))

(defn -main []
  (evaluate-query "fact1(c1,c2,c3)." "fact1(c1,c2,c3)")
 (print 2))
