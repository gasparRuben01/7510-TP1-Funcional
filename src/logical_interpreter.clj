(ns logical-interpreter
  (:require [preposition :refer :all]
            [rule :refer :all]
            [clojure.string :refer [split includes?]]
            [bool :refer :all]
            [rule :refer :all]))


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (try
    (let 
      [prepositions (atom {})
       consulta (atom nil)
       get-token (fn [string pattern usar-token no-tiene-token]
                  (if (and (re-find (re-matcher pattern string)) (get (re-find (re-matcher pattern string)) 1))
                    (usar-token (get (re-find (re-matcher pattern string)) 1))
                    (no-tiene-token)))

       error (fn [] (throw (Exception.)))
       pattern-name #"^\s*([^,\.\s\)]+)\s*$"
       pattern-var #"^\s*([^\s]+)\s*$"]

      (when (not (includes? database ".")) (error))
      (doseq [x (split database #"\.")]
        (when (not (re-find (re-matcher #"^\s+$" x)))
          (let 
            [preposition (atom nil)
             vars (atom {})
             pos (atom -1)
             token1 (atom nil)
             token2 (atom nil)
             split-tokens (fn [string pattern]
                            (when (or (not (re-find (re-matcher pattern string))) (> (count (split string pattern)) 2)) (error))
                            (swap! token1 (fn [c] (get (split string pattern) 0)))
                            (swap! token2 (fn [c] (get (split string pattern) 1))))
                          

             set-name (fn [str-name]
                        (if (contains? @prepositions str-name)
                          (swap! preposition (fn [x] (get @prepositions str-name)))
                          (swap! preposition (fn [x] (map->Preposition {:name str-name :facts #{} :rule nil})))))]

            (split-tokens x #"\(")
            (get-token @token1 pattern-name set-name error)
            (split-tokens @token2 #"\)")
            (doseq [y (split @token1 #",")]
              (let
                [set-variable (fn [str-var]
                                (if (contains? @vars str-var)
                                  (swap! vars assoc str-var (conj (get @vars str-var) (swap! pos inc)))
                                  (swap! vars assoc str-var [(swap! pos inc)])))]
                (get-token y pattern-var set-variable error)))
            
            (if (and (not (= token2 nil)) (includes? token2 ":-"))
              (do (split-tokens x "") nil)
              (do  
                  (let
                    [fact (atom [])]
                    (doseq [x (range (+ (reduce max (map (fn [x] (reduce max (get x 1))) @vars)) 1))] (swap! fact conj x))
                    (doseq [x @vars] (doseq [y (get x 1)] (swap! fact assoc y (get x 0))))
                    (swap! preposition (fn [p] (map->Preposition {:name (.name p)
                                                                  :rule (.rule p)
                                                                  :facts (conj (.facts p) @fact)}))))))
            (swap! prepositions assoc (.name @preposition) @preposition))))

      (let
        [q-name (atom nil)
         n-upla (atom [])
         set-name (fn [str-name]
                    (swap! q-name (fn [x] str-name)))]

         (get-token (get (split query #"\(") 0) pattern-name set-name error)
         (when (not (re-find (re-matcher #"\)\s*$" (get (split query #"\(") 1) ))) (error))
         (doseq [x (split (get (split (get (split query #"\(") 1) #"\)\s*$") 0) #",")]
           (let
             [set-var (fn [str-var] (swap! n-upla conj str-var))]
             (get-token x pattern-var set-var error)))
         (swap! consulta (fn [x] [@q-name @n-upla])))

      (if (contains? @prepositions (get @consulta 0))
        (bool (get @prepositions (get @consulta 0)) (get @consulta 1))
        false))

    (catch Exception e nil)))

    
