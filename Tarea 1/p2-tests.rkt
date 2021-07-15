#lang play
(require "t1.rkt")

; Tests typecheck
; Typecheck reconoce numeros y asigna Type Num
(test (typecheck '{3}) 'Num)
; Typecheck reconoce bools y asigna Type Bool
(test (typecheck '{#f}) 'Bool)
; Typecheck reconoce identificadores y busca su tipo en el env
; por eso da error de free identifier
(test/exn (typecheck '{x}) "free identifier")
; Acepta not y entrega Bool
(test (typecheck '{
                   {! #f}}) 'Bool)
; Verifica que el arg sea un Bool (o Any), con Num sea cae con Ste
(test/exn (typecheck '{
                   {! 10}}) "Static type error: expected Bool found Num")

; Acepta add1 y entrega Num
(test (typecheck '{
                   {add1 10}}) 'Num)
; Verifica que arg sea Num (o Any), con bool se cae con Ste
(test/exn (typecheck '{
                   {add1 #f}}) "Static type error: expected Num found Bool")

; Acepta sub1 y entrega Num
(test (typecheck '{
                   {sub1 10}}) 'Num)
; Verifica que arg sea Num (o Any), si no se cae con Ste
(test/exn (typecheck '{
                   {sub1 #f}}) "Static type error: expected Num found Bool")

; Acepta + y entrega Num
(test (typecheck '{
                   {+ 1 2}}) 'Num)
; Ste con bool
(test/exn  (typecheck '{
                   {+ #f 2}}) "Static type error: expected Num found Bool")

; Acepta - entrega Num
(test (typecheck '{
                   {- 1 2}}) 'Num)
; Ste con bool
(test/exn  (typecheck '{
                   {- #f 2}}) "Static type error: expected Num found Bool")

; Acepta * entrega Num
(test (typecheck '{
                   {* 1 2}}) 'Num)
; Ste con bool
(test/exn  (typecheck '{
                   {* #f 2}}) "Static type error: expected Num found Bool")

; Acepta / entrega Num
(test (typecheck '{
                   {/ 1 2}}) 'Num)
; Ste con bool
(test/exn  (typecheck '{
                   {/ #f 2}}) "Static type error: expected Num found Bool")

; Acepta and entrega Bool
(test (typecheck '{
                   {&& #f #f}}) 'Bool)
; Ste con num
(test/exn  (typecheck '{
                   {&& 2 #f}}) "Static type error: expected Bool found Num")

; Acepta = entrega Bool
(test (typecheck '{
                   {= 1 2}}) 'Bool)
; Ste con bool
(test/exn  (typecheck '{
                   {= #f 2}}) "Static type error: expected Num found Bool")

; Acepta < entrega Bool
(test (typecheck '{
                   {< 1 2}}) 'Bool)
; Ste con bool
(test/exn (typecheck '{
                   {< #f 2}}) "Static type error: expected Num found Bool")

; If verifica que primer arg sea Bool
(test (typecheck '{
                   {if #t #f #f}}) 'Bool)
; If entrega el tipo de ambas ramas
(test (typecheck '{
                   {if #t 10 10}}) 'Num)
; Ste si las ramas tienen tipos distintos
(test/exn (typecheck '{
                   {if #t 10 #f}}) "Static type error: expected both branches Bool or Num found Num and Bool")
; O si el arg no es Bool
(test/exn (typecheck '{
                   {if 10 #f #f}}) "Static type error: expected Bool found Num")

; Entrega tipo del cuerpo de with
(test (typecheck '{
                   {with {} 10}}) 'Num)
; Entrega tipo del cuerpo de with
(test (typecheck '{
                   {with {} #t}}) 'Bool)

; Entrega tipo del cuerpo de with, argumentos van con tipo Any default
(test (typecheck '{
                   {with {{x 10}} x}}) 'Any)
; Si uno le asigna un tipo se retorna ese
(test (typecheck '{
                   {with {{x : Num 10}} x}}) 'Num)
; Con bools funciona igual
(test (typecheck '{
                   {with {{y #f}} y}}) 'Any)
(test (typecheck '{
                   {with {{y : Bool #f}} y}}) 'Bool)
; Hay Ste si se declara tipo distintos al del valor de la variable
(test/exn (typecheck '{
                   {with {{y : Num #f}} y}}) "Static type error: expected Num found Bool")

; Se entrega el tipo del body, el que se obtiene de su typecheck
(test (typecheck '{
                   {with {{x 10} {y 20}} {+ x y}}}) 'Num)
; Si se definen tipos, estos se verifican en el cuerpo
(test/exn (typecheck '{
                   {with {{x : Bool 10} {y : Bool 20}} {+ x y}}})
          "Static type error: expected Bool found Num")
; Se verifican todos
(test/exn (typecheck '{
                   {with {{x 10} {y : Bool 20}} {+ x y}}})
          "Static type error: expected Bool found Num")
; Funciona igual con los Bool
(test (typecheck '{
                   {with {{x #t} {y #f}} {&& x y}}}) 'Bool)

; Funciones retornan el tipo que se les asigna
(test (typecheck '{
                   {define {foo} : Bool #f}
                   {foo}}) 'Bool)
; Se verifica qeu este tipo encaje con el cuerpo
(test/exn (typecheck '{
                   {define {foo} : Num #f}
                   {foo}}) "Static type error: expected Num found Bool")
; El tipo de las funciones se usa para el typecheck
(test/exn (typecheck '{
                   {define {foo} : Bool #f}
                   {+ {foo} {foo}}}) "Static type error: expected Num found Bool")
; Se puede engañar al typecheck con los Any (sin declarar tipo)
(test (typecheck '{
                   {define {foo} #f}
                   {+ {foo} {foo}}}) 'Num)

; Ahora errores de Aridad ocurren de forma estatica con el typecheck
(test/exn (typecheck '{
                   {define {foo x} : Bool x}
                   {foo}}) "Static error: the function foo requires more arguments")
; Si uno le pasa la cantidad justa no hay problemas
(test (typecheck '{
                   {define {foo x} : Bool x}
                   {foo #f}}) 'Bool)
; Si le da argumentos de mas tambien hay problemas
(test/exn (typecheck '{
                   {define {foo x} : Bool x}
                   {foo 10 10}}) "Static error: the function foo requires less arguments")
; Si uno define todos los tipos no deberia haber problemas en ejecucion
(test (typecheck '{
                   {define {foo {x : Bool}} : Bool x}
                   {foo #t}}) 'Bool)
; Al definir todo el typecheck es muy estricto, aqui revisa que tipo de arg
; de la funcion coincida con el tipo dado
(test/exn (typecheck '{
                   {define {foo {x : Bool}} : Bool x}
                   {foo 10}}) "Static type error: expected Bool found Num")
; Con varios argumentos tambien se revisa el tipo de cada argumento
(test/exn (typecheck '{
                   {define {foo {x : Bool} y} : Bool {&& x y}}
                   {foo 10 20}}) "Static type error: expected Bool found Num")
; Se verifica el cuerpo de la funcion, falla porque y se declaro como num
(test/exn (typecheck '{
                   {define {foo {x : Bool} {y : Num}} : Bool {&& x y}}
                   {foo 10 20}}) "Static type error: expected Bool found Bool and Num")
; Se checkean ambos argumentos, ambos fallan
(test/exn (typecheck '{
                   {define {foo {x : Bool} {y : Bool}} : Bool {&& x y}}
                   {foo 10 20}}) "Static type error: expected Bool found Num")
; Se puede engañar al typecheck con los Any, omiten una verificacion
; que luego provoca RE
(test (typecheck '{
                   {define {foo {x : Bool} y} : Bool {&& x y}}
                   {foo #t 20}}) 'Bool)

; El chequeo en una funcion no llega a otra mas arriba
(test (typecheck '{
                   {define {util x} : Bool x}
                   {define {foo x} : Bool {&& x {util x}}}
                   {foo 20}}) 'Bool)
(test (typecheck '{
                   {define {util {x : Bool}} : Bool x}
                   {define {foo x} : Bool {&& x {util x}}}
                   {foo 20}}) 'Bool)
; El chequeo solo llega a la que se llama
(test/exn (typecheck '{
                   {define {util {x : Bool}} : Bool x}
                   {define {foo {x : Bool}} : Bool {&& x {util x}}}
                   {foo 20}}) "Static type error: expected Bool found Num")

; Tests de enunciado
(test (typecheck '{3})
  'Num)  
(test (typecheck '{
                   {define {f {p : Bool}} {&& p {! p}}}
                   {f {< 3 4}}})
  'Any) 
(test/exn (typecheck '{
                       {define {one {x : Num}} 1}
                       {one #t}})
  "Static type error: expected Num found Bool")
(test/exn (typecheck '{{< 10 #t}})
  "Static type error")
(test/exn (typecheck '{{if 73 #t #t}})
  "Static type error: expected Bool found Num")
(test/exn (typecheck '{{with {{x 5} {y : Num #t} {z 42}}
                            z}})
  "Static type error: expected Num found Bool")

; Tests run
; solo se incluyen test que revisan buen/mal funcionamiento
; del typechecker, debido a que el buen funcionamiento de run
; se prueba con los test p1
; Ste con not y Num
(test/exn (run '{
                   {! 10}})
          "Static type error: expected Bool found Num")
; Ste con add1 y Bool
(test/exn (run '{
                   {add1 #f}})
          "Static type error: expected Num found Bool")
; Ste con sub1 y Bool
(test/exn (run '{
                   {sub1 #f}})
          "Static type error: expected Num found Bool")
; Ste con + y Bool
(test/exn  (run '{
                   {+ #f 2}})
           "Static type error: expected Num found Bool")
; Ste con - y Bool
(test/exn  (run '{
                   {- #f 2}})
           "Static type error: expected Num found Bool")
; Ste con * y Bool
(test/exn  (run '{
                   {* #f 2}})
           "Static type error: expected Num found Bool")
; Ste con / y Bool
(test/exn  (run '{
                   {/ #f 2}})
           "Static type error: expected Num found Bool")
; Ste con and y Num
(test/exn  (run '{
                   {&& 2 #f}})
           "Static type error: expected Bool found Num")
; Ste con = y Num
(test/exn  (run '{
                   {= #f 2}})
           "Static type error: expected Num found Bool")
; Ste con < y Num
(test/exn (run '{
                   {< #f 2}})
          "Static type error: expected Num found Bool")

; Si no se declaran tipos hay RE
(test/exn (run '{
                 {define {foo} #t}
                 {add1 {foo}}})
          "Runtime type error")
; Si se declaran se encuentran de forma estatica con un Ste
(test/exn (run '{
                 {define {foo} : Bool #t}
                 {add1 {foo}}})
          "Static type error: expected Num found Bool")

; Nuevamente, el tipo de una variable en una aplicacion solo llega
; hasta las varibles de la funcion, no hasta el body ni el tipo
; de la funcion
(test/exn (run '{
                 {define {foo x} : Num x}
                 {add1 {foo #t}}})
          "Runtime type error")
; Hay Ste si es que los tipos del body y la funcion no son compatibles
(test/exn (run '{
                 {define {foo {x : Bool}} : Num x}
                 {add1 {foo #t}}})
          "Static type error: expected Num found Bool")
; Hay Ste si le damos un argumento de un tipo no compatible
(test/exn (run '{
                 {define {foo {x : Num}} : Num x}
                 {add1 {foo #t}}})
          "Static type error: expected Num found Bool")
; Si damos bien los tipos no hay error
(test (run '{
                 {define {foo {x : Num}} : Num x}
                 {add1 {foo 10}}}) 11)

; If detecta tipo Any de foo y esta tranquilo hasta la ejecucion
(test/exn (run '{
             {define {foo} 10}
             {if {foo} 10 20}})
      "Runtime type error")
; Si decimos que foo en Num, se cae estaticamente Ste
(test/exn (run '{
             {define {foo} : Num 10}
             {if {foo} 10 20}})
      "Static type error: expected Bool found Num")
; Si definimos tipo Bool, se cae dinamicamente pues no hay tipo
; En el argumento
(test/exn (run '{
             {define {foo x} : Bool x}
             {if {foo 10} 10 20}})
      "Runtime type error")
; Si ponemos Num en argumento se cae por inconpatibilidad de tipos
; entre funcion y cuerpo
(test/exn (run '{
             {define {foo {x : Num}} : Bool x}
             {if {foo 10} 10 20}})
      "Static type error: expected Bool found Num")
; Si definimos ambos Bool, se cae porque le damos un argumento Bool
; Ste
(test/exn (run '{
             {define {foo {x : Bool}} : Bool x}
             {if {foo 10} 10 20}})
      "Static type error: expected Bool found Num")

; Si no definimos el tipo en funcion que usan operadores se cae
; igual, porque se asume tipo Any en argumentos
(test/exn (run '{
                 {define {> x y} {&& {! {< x y}} {! {= x y}}}}
                 {> #f 20}})
          "Runtime type error")
; El tipo de retorno de and en Bool, por lo que no hay problemas
; de tipo y se vuelve a caer dinamicamente
(test/exn (run '{
                 {define {> x y} : Bool {&& {! {< x y}} {! {= x y}}}}
                 {> #f 20}})
          "Runtime type error")
; Si aclaramos un tipo Bool, se cae porque operadores esperan Num
(test/exn (run '{
                 {define {> {x : Bool} y} {&& {! {< x y}} {! {= x y}}}}
                 {> #f 20}})
          "Static type error: expected Num found Bool")
; Si forzamos ambos operadores como Num, se cae porque damos un Bool
; al aplicar la funcion
(test/exn (run '{
                 {define {> {x : Num} {y : Num}} {&& {! {< x y}} {! {= x y}}}}
                 {> #f 20}})
          "Static type error: expected Num found Bool")

; Nuevamente, si no definimos tipo de argumentos, podemos engañar
; al typecheck y llegar a un error de ejecucion
(test/exn (run '{
                 {with {{x 10}} {&& x #t}}})
          "Runtime type error")
; Si declaramos el tipo de un variable del with, esta repercute en su
; body, generando un Ste con and
(test/exn (run '{
                 {with {{x : Num 10}} {&& x #t}}})
          "Static type error: expected Bool found Num")
; Si damos un tipo incompatible con el declarado en la variable se
; cae Ste
(test/exn (run '{
                 {with {{x : Bool 10}} {&& x #t}}})
          "Static type error: expected Bool found Num")

; Test de enunciado
(test (run '{
             {with {{x : Num 5} {y : Num 10}}
                   {+ x y}}}) 15)
(test (run '{
             {define {gt42 x} : Bool {< x 42}}
             {gt42 43}}) #f) 
(test (run '{
             {define {id {x : Num}} x}
             {id 5}}) 5)
(test/exn (run '{
             {define {add2 {x : Num}} {+ x 2}}
             {with {{oops #f}}
                   {add2 oops}}}) "Runtime type error")
(test/exn (run '{
             {define {add2 {x : Num}} {+ x 2}}
             {with {{oops : Bool #f}}
                   {add2 oops}}}) "Static type error")