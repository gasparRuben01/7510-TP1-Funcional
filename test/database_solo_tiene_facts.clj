(ns database-solo-tiene-facts
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def database-only-facts
  "fact1(c1, c2, c3).
   fact2 (c4, c5, c7).
   fact3 (c1,    c8).
   fact2 (c8, c7, c4).")

(def database-easy
  "fact1(c1,c2,c3).")

(deftest database-easy-test
  (testing "facil"
    (is (= (evaluate-query database-easy "fact1(c1,c2,c3)") true))))

(deftest database-only-facts-test
  (testing "fact1(c1, c2, c3) deberia ser cierto"
    (is (= (evaluate-query database-only-facts "fact1(c1, c2, c3)") true)))
  (testing "debe ignorar espacios en blanco, por lo que fact1  (  c1, c2,    c3   ), tambien debe ser cierto"
    (is (= (evaluate-query database-only-facts " fact1  (  c1, c2,    c3  )") true)))
  (testing "fact2(c8, c7, c4) , fact2(c4, c5, c7), fact3(c1, c8)  deben ser ciertos"
    (is (= (and (evaluate-query database-only-facts "fact2(c8, c7, c4)") (evaluate-query database-only-facts "fact2(c8, c7, c4)") (evaluate-query database-only-facts "fact2(c8, c7, c4)")) true))))
