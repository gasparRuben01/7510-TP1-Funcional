(ns logical-interpreter
  (:require [preposition :refer :all]
            [rule :refer :all]))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (let 
    [pattern-name #"^\s*([^\(,.\s]+)\s*\("         
     pattern-no-last-variable #"^\s*([^,\s\)]+)\s*,"
     pattern-last-variable #"^\s*([^,\s\)]+)\s*\)"
     pattern-rule? #"^\s*:-"
     pattern-end #"^\s*.\s*"
     get-name (fn
                [string]
                (let
                  [match-name (re-find (re-matcher patter-name string))]
                  :



     parse-database (fn
                      [string]
                      (let
                         function-string (atom nil)
                         preposition (atom (map->Preposition :name nil :facts #{} :rule nil))
                         pos (atom -1)
                         variables (atom {})
                         prepositions (atom {})

                        end (fn [string]
                              (let
                                [match (re-find (re-matcher pattern-end string))
                                 token 0
                                 rest-string (atom nil)]

                                (if match
                                  (do
                                    (swap! prepositions (fn [x] (assoc x (:name @preposition) @preposition)))
                                    (swap! pos (fn [x] -1))
                                    (swap! rest-string (fn [x] (replace-first x (get match token ""))))
                                    (if (= @rest-string "")
                                      nil
                                      [set-preposition-name @rest-string]))
                                  (do (swap! prepositions (fn [x] {})) nil))))
                         
                         rule? (fn [string]
                                 (let
                                   [match (re-find (re-matcher pattern-rule? string))
                                    fact (atom [])]

                                   (if match
                                     nil
                                     (do
                                       (doseq [x @variables] (swap! fact (fn [y] (conj y (get x 0)))))
                                       (swap! preposition (fn [x] (map->Preposition {:name (.name x) :rule (.rule x) :facts (assoc (.facts x) @vars)})))
                                       [end string])))

                        add-last-variable (fn [string]
                                            (let
                                              [match (re-find (re-matcher pattern-last-variable string))
                                               token 0
                                               variable 1]
                                              (if
                                                (cond
                                                  (= match nil) nil
                                                  :else (swap! variables (fn [x] (assoc x  (get match variable) (swap! pos inc)))))
                                                [rule? (replace-first string (get match token) "")]
                                                (do (swap! prepositions (fn [x] {})) nil))))

                         add-variable (fn [string]
                                        (let
                                          [match (re-find (re-matcher pattern-no-last-variable string))
                                           token 0
                                           variable 1]
                                          (if
                                            (cond
                                              (= match nil) nil
                                              :else (swap! variables (fn [x] (assoc x  (get match variable) (swap! pos inc)))))
                                            [add-variable (replace-first string (get match token) "")]
                                            [add-last-variable string])))

                         set-preposition-name (fn [string]
                                                (let
                                                  [match (re-find (re-matcher pattern-name string))
                                                   token 0
                                                   prepo-name 1]
                                                  (if
                                                    (cond
                                                      (= match nil) nil
                                                      (contains? @prepositions (get match prepo-name)) (swap! prepostion (fn [x] (get @prepositions (get match prepo-name))))
                                                      :else (swap! preposition (fn [x] (map->Preposition {:name (get match prepo-name) :facts #{} }))))

                                                    [add-variable (replace-first string (get match token) "")]
                                                    (do (swap! prepositions (fn [x] {}) nil)))))

                        (swap! function-string (fn [x] [set-preposition-name string]))
                        (while (boolean @function-string)
                          (swap! function-string (fn [x] ((get x 0) (get x 1)))))
                        @prepositions)
     parse-query (fn [string]
                   (let [preposition atom(nil)
                         n-upla atom(nil)]
                     (swap! preposition (fn [x] 
