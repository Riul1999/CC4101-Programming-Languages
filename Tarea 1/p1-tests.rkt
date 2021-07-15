#lang play
(require "t1.rkt")

; Tests Run
; Se ejecuta el programa con solo numeros
(test (run '{
            {1}}) 1)
; Se ejecuta el programa con solo booleanos
(test (run '{
            {#f}}) #f)

; El programa que niega un booleano funciona
(test (run '{
            {! #f}}) #t)
; El programa que niega un numero falla con Runtime error
(test/exn (run '{
            {! 10}} #f) "not-pro: Runtime type error")

; El programa que hace add1 a un numero funciona
(test (run '{
             {add1 10}}) 11)
; El programa que hace add1 a un booleano falla con Runtime error
(test/exn (run '{
             {add1 #t}} #f) "add1-pro: Runtime type error")

; El programa que hace sub1 a un numero funciona
(test (run '{
             {sub1 10}}) 9)
; El programa que hace sub1 a un booleano falla con Runtime error
(test/exn (run '{
             {sub1 #f}} #f) "sub1-pro: Runtime type error")

; Adicion funciona con numero
(test (run '{
             {+ 1 2}}) 3)
; Se cae con bools, con un Runtime error
(test/exn (run '{
             {+ #f 2}} #f) "sum-pro: Runtime type error")

; Funciona resta
(test (run '{
             {- 1 2}}) -1)
; Se cae con bools, con un Runtime error
(test/exn (run '{
             {- #f 2}} #f) "res-pro: Runtime type error")

; Funciona multiplicacion
(test (run '{
             {* 1 2}}) 2)
; Se cae con bools, con un Runtime error
(test/exn (run '{
             {* #f 2}} #f) "mul-pro: Runtime type error")

; Funciona la division
(test (run '{
             {/ 1 2}}) 1/2)
; Se cae con bools, RE
(test/exn (run '{
             {/ #f 2}} #f) "div-pro: Runtime type error")

; Funciona el and entre bools
(test (run '{
             {&& #t #t}}) #t)
; Se cae con numeros, RE
(test/exn (run '{
             {&& #f 2}} #f) "and-pro: Runtime type error")

; Funciona el igual entre numeros
(test (run '{
             {= 10 20}}) #f)
(test (run '{
             {= 10 10}}) #t)
; Se cae con bools, RE
(test/exn (run '{
             {= #f 2}} #f) "eq-pro: Runtime type error")

; Funciona el lt entre numeros
(test (run '{
             {< 10 20}}) #t)
; Se cae con bools, RE
(test/exn (run '{
             {< #f 2}} #f) "lt-pro: Runtime type error")

; If funcion con primer argumento booleano
(test (run '{
             {if #f #t #f}}) #f)
(test (run '{
             {if #f #t 20}} #f) 20)

; Se cae si el primer argumento en numero
(test/exn (run '{
             {if 10 #t #f}} #f) "Runtime type error")
; No parsea bien si se dan menos o mas argumentos (cae en
; caso de las aplicaciones de funcion)
(test/exn (run '{
             {if #t #f}}) "undefined function: 'if")

; Se puede usar with sin expresiones
(test (run '{
                 {with {} 2}}) 2)
; Con una expresion
(test (run '{
                 {with {{x 3}} x}}) 3)
; Con mas de una expresion
(test (run '{
                 {with {{x 3} {x 4}} x}}) 4)
; Se pueden usar las expresiones definidas en un with
; con otras funciones
(test (run '{
                 {with {{x 3} {y 4}} {+ x y}}}) 7)
; Se pueden usar with anidados
(test (run '{
             {with {{x 3}}
                   {with {{y x}} {+ y y}}}}) 6)
; Se busca en el ambiente para las expresiones del with
; no se usa la actual (entraria en un loop infinito)
(test (run '{
             {with {{x 3}}
                   {with {{x x}} {+ x x}}}}) 6)
; Se utiliza unicamente la ultima definicion de la variable
(test (run '{
             {with {{x 3}}
                   {with {{x 4}} {+ x x}}}}) 8)

; Se busca en el ambiente el x (no se usa el que se esta
; definiendo) lo que produce un error
(test/exn (run '{
             {with {{x x}} x}}) "free identifier")
; Si no se entrega el valor de una variable de with, PE
(test/exn (run '{
                 {with {{x 10} {y}} {+ x y}}}) "Parse error")
; Si no se entrega el identificador de una variable de with, PE
(test/exn (run '{
                 {with {{x 10} {10}} {+ x y}}}) "Parse error")
; Si no se entrega el cuerpo de with, cae en caso de aplicacion
; de funcion
(test/exn (run '{
                 {with {{x 10} {y 10}} }}) "undefined function: 'with")
; Se pueden definir funciones con multiples argumentos y
; ocuparlas en el programa principal
(test (run '{
             {define {sum-mul x y z} {+ x {* y z}}}
             {sum-mul 10 20 30}}) 610)
; Se pueden definir funciones sin argumentos
(test (run '{
             {define {ten} 10}
             {ten}}) 10)
; Se pueden definir funciones con un argumento y usarlas en with
(test (run '{
             {define {mul-3 x} {* x 3}}
             {with {{x 10}}
                   {+ x {mul-3 2}}}}) 16)
; Se puede usar una funcion en otra, incluso aunque este definida
; "luego"
(test (run '{
             {define {mul-3 x} {* x {add2 1}}}
             {define {add2 x} {+ x 2}}
             {with {{x 10}}
                   {+ x {mul-3 2}}}}) 16)

; Se interpreta el cuerpo de las funciones
(test/exn (run '{
                 {define {mult-3 x} {+ x y}}
                 {mult-3 2}}) "free identifier")
; Hay error incluso aunque se defina la varible que falta
; Scope estatico
(test/exn (run '{
                 {define {mult-3 x} {+ x y}}
                 {with {{y 2}} {mult-3 2}}}) "free identifier")
; Si no se agrega body a una funcion hay PE
(test/exn (run '{
                 {define {mul-3 x} }
                 {mult-3 2}}) "Parse error")
; Si no le entregamos argumentos suficientes a una funciones, RE
(test/exn (run '{
                 {define {mul-3 x} {* 3 x}}
                 {mul-3}} #f) "Runtime error")
; Si le entregamos mas argumentos de los requeridos a una funcion, RE 
(test/exn (run '{
                 {define {mul-3 x} {* 3 x}}
                 {mul-3 2 3}} #f) "Runtime error")

;programas de ejemplo
(test (run
'{ ;; Programa de Ejemplo 1
   {define {sum x y z} {+ x {+ y z}}}
   {define {max x y} {if {< x y} y x}}
   {with {{x 9}}
        {sum {max x 6} 2 -10} }
}) 1)
(test (run
'{ ;; Programa de Ejemplo 2
   {with {{x 5} {y 7} {z 42}}
         z}
}) 42)
(test (run
'{ ;; Programa de Ejemplo 3
   {define {triple x} {* 3 x}}
   {define {add2 x} {+ 2 x}}
   {add2 {triple 2}}
}) 8)
(test/exn (run '{
            {+ 1 #f}} #f) "Runtime type error")

; Caso en que el interprete no puede parsear lo ingresado
(test/exn (interp (parse "hola") '() empty-env) "Parse error")
; Si usamos una funcion no definada hay undefined function
(test/exn (run '{
                 {add2 10}}) "undefined function: 'add2")


