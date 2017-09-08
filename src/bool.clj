(ns bool)

(defprotocol Bool
  "los tipos que implementen este protocolo deben poder retornar un valor de verdad"
  (bool [x y] "retorna del valor de verda de x evaluado en y"))
