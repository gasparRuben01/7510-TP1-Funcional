(ns preposition-and-rule-test 
  (:require [clojure.test :refer :all]
            [bool :refer :all]
            [preposition :refer :all]
            [rule :refer :all ]))

(deftest preposition-rule-test-evaluation
  (testing "evaluar preposition con una n-upla contenida en sus facts debe retornar true"
    (def preposition (map->Preposition { :name "p" :facts #{ [ 1 2 3 ] [ 2 3 4 ] } :rule nil }))
    (is (= (bool preposition [ 1 2 3 ]) true)))

  (testing "evaluar rule con n-upla correcta debe dar true"
    (def preposition (map->Preposition { :name "p" :facts #{ [ 2 1 3 ] } :rule nil }))
    (def rule (map->Rule { :preposition preposition :order [1 0 2] }))
    ;;notar que la tupla con la que se evalua rule es distinta a la tupla que tiene preposition en facts, 
    ;;pero rule reordena la tupla usando las posiciones de order antes de evaluar la preposicion
    (is (= (bool rule [1 2 3]) true)))

  (testing "evaluar AndRule con n-upla correcta debe dar true"
    (def preposition1 (map->Preposition { :name "p1" :facts #{ [2 1 3 ]} :rule nil } ))
    (def rule1 (map->Rule { :preposition preposition1 :order [1 0 2] }))

    (def preposition2 (map->Preposition { :name "p2" :facts #{ [5 9 0 ] [3 1 2] } :rule nil } ))
    (def rule2 (map->Rule { :preposition preposition2 :order [2 0 1] } ))
    
    (def and_rule (map->AndRule { :rules [rule1 rule2] }))

    (is (= (bool and_rule [1 2 3]) true)))

  (testing "evaluar preposition con n-upla que no es un fact pero si hace verdad a rule retorna true"
    (def preposition1 (map->Preposition { :name "p1" :facts #{ [2 1 3 ]} :rule nil } ))
    (def rule1 (map->Rule { :preposition preposition1 :order [1 0 2] }))

    (def preposition2 (map->Preposition { :name "p2" :facts #{ [5 9 0 ] [3 1 2] } :rule nil } ))
    (def rule2 (map->Rule { :preposition preposition2 :order [2 0 1] } ))
    
    (def and_rule (map->AndRule { :rules [rule1 rule2] }))
    (def preposition_a_evaluar (map->Preposition { :name "preposition" :facts #{ [8 99] } :rule and_rule } ))

    (is (= (bool preposition_a_evaluar [1 2 3]) true))))
