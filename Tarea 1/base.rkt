#lang play

#|
1- Entender lo que hace la funcion
2- Contrato
3- Descripcion
4- Tests
5- Implementar
|#

; double-list :: (Listof Number) -> (Listof Number)
; duplica los elementos de la lista
; lanza un error si el parámetro no es una lista
(define (double-list l)
  (if (list? l)
      (map (λ (x) (* x 2)) l)
      (error "ERROR: not a list!!!!")))
 
(test (double-list '(1 2 3)) '(2 4 6))
(test (double-list empty) empty)
(test/exn (double-list 4) "not a list")

#| <nat> ::= 0
          |  ( add1 ‹nat› )
|#
