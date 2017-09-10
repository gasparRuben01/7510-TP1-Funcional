(ns logical-interpreter
  (:require [preposition :refer :all]
            [rule :refer :all]
            [clojure.string :refer [replace-first]]
            [bool :refer :all]))


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (let 
    [prepositions (atom nil)
     consulta (atom nil)
     pattern-name #"^\s*([^\(,.\s]+)\s*\("         
     pattern-no-last-variable #"^\s*([^,\s\)]+)\s*,"
     pattern-last-variable #"^\s*([^,\s\)]+)\s*\)"
     pattern-rule? #"^\s*:-"
     pattern-end #"^\s*.\s*"
     first-token (fn [pattern string]
                   (if (re-find (re-matcher pattern string))
                     (get (re-find (re-matcher pattern string) 1))
                     nil))

     delete-first-match (fn [pattern string] (replace-first string pattern ""))

     get-name (fn [string] (first-token pattern-name string))
     delete-name (fn [string] (delete-first-match pattern-name string))
     
     get-no-last-var (fn [string] (first-token pattern-no-last-variable string))
     delete-no-last-var (fn [string] (delete-first-match pattern-no-last-variable string))

     get-last-var (fn [string] (first-token pattern-last-variable string))
     delete-last-var (fn [string] (delete-first-match pattern-last-variable string))

     get-end (fn [string] (first-token pattern-end string))
     delete-end (fn [string] (first-token pattern-end string))

     rule? (fn [string] (first-token pattern-rule? string))
     delete-rule-symbol (fn [string] (delete-first-match pattern-rule? string))

     parse-rule (fn [x] nil)

     parse-vars (fn [string add-var]
                  (let
                    [buffer (atom string)
                     var-name (atom nil)]
                    (swap! var-name get-no-last-var @buffer)
                    (while @var-name
                      (add-var @var-name)
                      (swap! buffer delete-no-last-var)
                      (swap! var-name get-no-last-var @buffer))
                    
                    (swap! var-name get-last-var @buffer)
                    (when (not @var-name) (throw (Exception.)))
                    (add-var @var-name)
                    (swap! buffer delete-last-var)
                    @buffer))




     parse-database (fn
                       [database]
                       (let
                         [string (atom database)
                          prepositions (atom {})
                          continue-bucle (atom true)
                          vars (atom {})]

                         (while @continue-bucle
                          (let
                           [preposition (atom nil)
                            prep-name (get-name @string)]

                            (print "----" prep-name) 

                           (if (prep-name) 
                             (if (contains? @prepositions prep-name)
                               (swap! preposition (fn [x] (get @prepositions prep-name)))
                               (swap! preposition (fn [x] (map->Preposition { :name prep-name }))))
                             (throw (Exception.)))
                           (swap! string delete-name) 

                            (let
                               [pos (atom -1)
                                add-var (fn [var-name] 
                                          (if (contains? @vars var-name)
                                            (swap! vars (fn [v] (assoc v var-name (conj (get v var-name) (swap! pos inc)))))
                                            (swap! vars assoc var-name [(swap! pos inc)])))]
                              (swap! string parse-vars add-var))

                            (if (rule? @string)
                              (parse-rule @string)
                              (do
                                  (if (get-end @string)
                                    (do 
                                      (let
                                        [fact (atom [])]
                                        (swap! string delete-end)
                                        (swap! fact into (range (+ (reduce max (map (fn [x] (reduce max (get x 1))) @vars)) 1)))
                                        (doseq [x @vars] (doseq [y (get x 1)] (swap! fact assoc y (get x 0))))
                                        (swap! preposition (fn [x] 
                                                             (map->Preposition {:name (.name x)
                                                                                :rule (.rule x)
                                                                                :facts (conj (.facts x) @fact)})))
                                        (swap! prepositions assoc (.name @preposition) @preposition)))
                                    (throw (Exception.)))))
                            (when (= @string "") (swap! continue-bucle (fn[x] false)))))

                         @prepositions))

     parse-query (fn [query]
                   (let 
                     [buffer (atom query)
                      query-name (get-name buffer) 
                      query-nupla (atom nil)
                      vars (atom [])
                      add-vars (fn [var-name] (swap! vars conj var-name))]

                     (if query-name
                       (do
                         (swap! buffer parse-vars add-vars)
                         [@query-name @vars])
                       (throw (Exception.)))))]
     (try
       (do
         (swap! prepositions (fn [x] (parse-database database)))
         (swap! consulta (fn [x] (parse-query query)))
         (bool (get prepositions (get consulta 0)) (get consulta 1)))
       (catch Exception e nil))))
       
