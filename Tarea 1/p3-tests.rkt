#lang play

(require "t1.rkt")

; Tests run
; solo se prueba la funcionalidad estica y dinamica de los
; los contratos, el resto de las funcionalidades de run se
; prueban con los test de p1 y p2

; contratos permiten checkeo dinamico de los argumetos de funciones
(test/exn (run '{
                 {define {verdadero? x} : Bool x}
                 {define {y-solo-trues {x @ verdadero?} {y : Bool @ verdadero?}} {&& x y}}
                 {y-solo-trues #t #f}})
          "Runtime contract error: #f does not satisfy verdadero?")
; contrato es valido solo si el tipo es Bool (estaticamente)
(test/exn (run '{
                 {define {verdadero? x} : Num x}
                 {define {y-solo-trues {x @ verdadero?} {y : Bool @ verdadero?}} {&& x y}}
                 {y-solo-trues #t #f}})
          "Static contract error: invalid type or args length for verdadero?")
; contrato no acepta tipo Any, solo Bool (estaticamente)
(test/exn (run '{
                 {define {verdadero? x} x}
                 {define {y-solo-trues {x @ verdadero?} {y : Bool @ verdadero?}} {&& x y}}
                 {y-solo-trues #t #f}})
          "Static contract error: invalid type or args length for verdadero?")
; contrato invalido si requiere mas de un argumento (estaticamente)
(test/exn (run '{
                 {define {verdadero? x y} : Bool x}
                 {define {y-solo-trues {x @ verdadero?} {y : Bool @ verdadero?}} {&& x y}}
                 {y-solo-trues #t #f}})
          "Static contract error: invalid type or args length for verdadero?")
; contrato invalido si requiere menos de un argumento (estaticamente)
(test/exn (run '{
                 {define {verdadero?} : Bool #t}
                 {define {y-solo-trues {x @ verdadero?} {y : Bool @ verdadero?}} {&& x y}}
                 {y-solo-trues #t #f}})
          "Static contract error: invalid type or args length for verdadero?")
; se puede usar un contrato para validar otro
(test (run '{
             {define {> x y} {&& {! {< x y}} {! {= x y}}}}
             {define {positive? x} : Bool {> x 0}}
             {define {gt2? {x @ positive?}} : Bool {> x 2}}
             {define {sum-gt2 {x @ gt2?} {y @ positive?}} {+ x y}}
             {sum-gt2 3 2}}) 5)
; se cae si se incumple cualquiera de los dos
(test/exn (run '{
             {define {> x y} {&& {! {< x y}} {! {= x y}}}}
             {define {positive? x} : Bool {> x 0}}
             {define {gt2? {x @ positive?}} : Bool {> x 2}}
             {define {sum-gt2 {x @ gt2?} {y @ positive?}} {+ x y}}
             {sum-gt2 1 2}})
      "Runtime contract error: 1 does not satisfy gt2")
(test/exn (run '{
             {define {> x y} {&& {! {< x y}} {! {= x y}}}}
             {define {positive? x} : Bool {> x 0}}
             {define {gt2? {x @ positive?}} : Bool {> x 2}}
             {define {sum-gt2 {x @ gt2?} {y @ positive?}} {+ x y}}
             {sum-gt2 3 -1}})
      "Runtime contract error: -1 does not satisfy positive?")
; los contratos tambien se pueden usar como funcion
(test (run '{
             {define {neg? x} : Bool {< x 0}}
             {define {positive {y @ neg?}} {* -1 y}}
             {neg? {positive -1}}}) #f)
; si se usa un contrato no definido es como usar una funcion no definida
(test/exn (run '{
             {define {positive {y @ neg?}} {* -1 y}}
             {positive -1}})
          "undefined function")

; test enunciado
(test (run '{
             {define {> x y} {&& {! {< x y}} {! {= x y}}}}
             {define {positive x} : Bool {> x 0}}
             {define {div {x : Num @ positive} y} {/ y x}}
             {div 5 3}}) 3/5)
(test (run '{
             {define {> x y} {&& {! {< x y}} {! {= x y}}}}
             {define {lt100 x} {< x 100}}
             {define {positive x} : Bool {> x 0}}
             {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
             {define {calc {x @ positive} {y @ percentage?}} {/ {* y y} x}}
             {calc 25 3}}) 9/25)
(test/exn (run '{
                 {define {add x y} : Num {+ x y}}
                 {define {oh-no {x @ add} y} #t}
                 {oh-no 21 21}})
"Static contract error: invalid type or args length for add")
(test/exn (run '{
                 {define {add x y} : Bool {&& x y}}
                 {define {oh-no {x @ add} y} #t}
                 {oh-no 21 21}})
"Static contract error: invalid type or args length for add")