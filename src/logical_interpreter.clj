(ns logical-interpreter)


(defprotocol Bool
  "los objetos que implementen este protocolo  valor de verdad asociado"
  (bool [x] "retorna valor de verdad cuando el dato se evalua con x"))

;;facts es un set de n-uplas que se saben que son ciertas, mientras rule
;;es una colleccion de reglas que definen la vericidad de facts
(defrecord preposition facts rule
  Bool
  (bool [x]
        "una prepostion is true si x es un hecho o las regla asociada a x se evalua cierta"
        (or (contains? facts x) (bool rule x))))

(defrecord rule
  Bool
  (bool [x]

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]

  (defn next
    [delimitador]
    (delimitador {"(" openParenthesis
                  ")" closeParenthesis
                  "." period
                  ":" colo
                  "-" dash})
   (def match (.match #"[().:-]) database) 
   
  nil)
