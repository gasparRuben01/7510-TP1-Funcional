(ns logical-interpreter
  (:require [preposition :refer :all]
            [rule :refer :all]
            [clojure.string :refer [replace-first]]
            [bool :refer :all]
            [rule :refer :all]))


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (let 
    [prepositions (atom nil)
     consulta (atom nil)
     pattern-name #"^\s*([^\(,\.\s]+)\s*\("         
     pattern-no-last-variable #"^\s*([^,\s\)\.]+)\s*,"
     pattern-last-variable #"^\s*([^,\s\)\.]+)\s*\)"
     pattern-rule? #"^\s*:-"
     pattern-end #"^\s*(\.)\s*"
     pattern-next-rule? #"^\s*,"

     first-token (fn [pattern string]
                   (if (re-find (re-matcher pattern string))
                     (get (re-find (re-matcher pattern string)) 1)
                     nil))

     delete-first-match (fn [pattern string] (replace-first string pattern ""))

     get-name (fn [string] (first-token pattern-name string))
     delete-name (fn [string] (delete-first-match pattern-name string))
     
     get-no-last-var (fn [string] (first-token pattern-no-last-variable string))
     delete-no-last-var (fn [string] (delete-first-match pattern-no-last-variable string))

     get-last-var (fn [string] (first-token pattern-last-variable string))
     delete-last-var (fn [string] (delete-first-match pattern-last-variable string))

     get-end (fn [string] (first-token pattern-end string))
     delete-end (fn [string] (delete-first-match pattern-end string))

     rule? (fn [string] (first-token pattern-rule? string))
     delete-rule-symbol (fn [string] (delete-first-match pattern-rule? string))

     next-rule? (fn [string] (first-token pattern-next-rule? string))
     delete-next-rule-symbol (fn [string] (delete-first-match pattern-next-rule?n string)) 

     parse-vars (fn [string add-var]
                  (let
                    [buffer (atom string)
                     var-name (atom nil)]
                    (swap! var-name (fn [x] (get-no-last-var @buffer)))
                    (while @var-name
                      (add-var @var-name)
                      (swap! buffer delete-no-last-var)
                      (swap! var-name (fn [x] (get-no-last-var @buffer))))
                    
                    (swap! var-name (fn [x] (get-last-var @buffer)))
                    (when (not @var-name) (throw (Exception.)))
                    (add-var @var-name)
                    (swap! buffer delete-last-var)
                    @buffer))

     parse-rule (fn [string vars prepositions ptr-preposition]
                  (let
                    [buffer (atom string)
                     rules (atom [])
                     continue-bucle (atom true)]

                    (while @continue-bucle
                      (let
                        [pre-name (get-name @buffer)
                         order (atom [])
                         add-order (fn [var-name]
                                     (swap! order conj (get vars var-name)))]

                        (when (or (not pre-name) (not (contains? prepositions pre-name)))
                          (throw (Exception.)))
                        (swap! buffer delete-name)
                        (swap! buffer parse-vars add-order)
                        (swap! rules conj (map->Rule { :preposition (get prepositions pre-name) :order @order}))
                        (if (next-rule? @buffer)
                          (swap! buffer delete-nex-rule-symbol)
                          (swap! continue-bucle (fn[x] false)))))
                    (swap! ptr-preposition (fn [p] (map->Preposition { :name (.name p) :facts (.facts p) :rule (map->AndRule { :rules @rules})})))
                    @buffer)) 
                        


                    

     parse-database (fn
                       [database]
                       (let
                         [string (atom database)
                          prepositions (atom {})
                          continue-bucle (atom true)]

                         (while @continue-bucle
                          (let
                           [preposition (atom nil)
                            prep-name (get-name @string)
                            vars (atom {})]

                           (if prep-name 
                             (if (contains? @prepositions prep-name)
                               (swap! preposition (fn [x] (get @prepositions prep-name)))
                               (swap! preposition (fn [x] (map->Preposition { :name prep-name :facts #{}}))))
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
                              (do
                                (swap! string delete-rule-symbol)
                                (swap! string parse-rule  @vars @prepositions preposition)
                                (when (not (get-end @string)) (throw (Exception.)))
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
                                                                                :facts (conj (.facts x) @fact)})))))
                                    (throw (Exception.)))))
                                (swap! prepositions assoc (.name @preposition) @preposition)
                            (when (= @string "") (swap! continue-bucle (fn[x] false)))))

                         @prepositions))

     parse-query (fn [query]
                   (let 
                     [buffer (atom query)
                      query-name (get-name @buffer) 
                      query-nupla (atom nil)
                      vars (atom [])
                      add-vars (fn [var-name] (swap! vars conj var-name))]

                     (swap! buffer delete-name)
                     (if query-name
                       (do (swap! buffer parse-vars add-vars) [query-name @vars])
                       (throw (Exception.)))))]
     (try
       (do
         (swap! prepositions (fn [x] (parse-database database)))
         (swap! consulta (fn [x] (parse-query query)))
         (bool (get @prepositions (get @consulta 0)) (get @consulta 1)))
       (catch Exception e nil))))
       
