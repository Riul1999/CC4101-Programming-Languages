#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;; Ejemplos del enunciado
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo () 5)))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

(test (run-val '(local
                  [(define seller (object
                                   (method multiplier () 1)
                                   (method price (item-number)
                                           (* item-number (send this multiplier)))))
                   (define broker (object : seller
                                          (method multiplier () 2)))]
                  (send broker price 3)))
      6)

(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

(test/exn (run-val '(local
                        ([define x (object
                                    (field z 3)
                                    (method get () (get z)))]
                         [define y (object
                                    : x
                                    (method get () (get z)))])
                      (send y get)))
          "field not found")

(test (run-val '(local
	([define x (object
				(field f 3)
				(method f-getter () (get f))
				(method get () (send z msg)))]
	[define y (object : x
				(field f 5)
				(method msg () (send this f-getter))
				(method f-getter () (get f)))]
	[define z (object : y
				(field f 8)
				(method f-getter () (get f)))])
				(send y get)))
      8)

(test/exn (run-val '(local
               ([define x y]
                [define y z]
                [define z 2]) 
             x))
          "free identifier")

;; A simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parte 1

(define test-obj '(object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))
                          (method get-x () (get x))
                          (method fail-get () (get h))
                          (method fail-set (r) (set h r))
                          (method this-set-x (val) (send this set-x val))
                          (method this-sum (val) (send this sum val))
                          (method sum-vals (x y z) (+ x (+ y z)))
                          ))

;; Test uso get fuera de objeto
(test/exn (run-val
       '(+ 2 (get x)))
      "Static error: 'get' used outside of an object")

;; Test uso this fuera de objeto
(test/exn (run-val
       '(+ 2 (send this 'fun 2)))
      "Static error: 'this' used outside of an object")

;; Test uso set fuera de objeto
(test/exn (run-val
       '(+ 2 (set h 2)))
      "Static error: 'set' used outside of an object")

;; Test obtener field que existe ((get x) y (get y))
(test (run-val
       `(local ([define o ,test-obj ])
          (send o sum 10)))
      13)

;; Test cambiar un campo existente (set x 9)
(test (run-val
       `(local ([define o ,test-obj])
          (seqn
           (send o set-x 9)
           (send o sum 10))))
      21)

;; Test método sin parámetros
(test (run-val
       `(local ([define o ,test-obj])
           (send o get-y)))
      2)

;; Test método con más de 1 parámetro
(test (run-val
       `(local ([define o ,test-obj])
           (send o sum-vals 1 2 3)))
      6)

;; Test uso de this
(test (run-val
       `(local ([define o ,test-obj])
          (seqn
           (send o this-set-x 9)
           (send o this-sum 10))))
      21)

;; Test get a parámetro no existente
(test/exn (run-val
       `(local ([define o ,test-obj])
          (send o fail-get)))
      "field not found")

;; Test set parámetro no existente
(test/exn (run-val
       `(local ([define o ,test-obj])
          (send o fail-set 12)))
          "field not found")

;; Test método no existente
(test/exn (run-val
       `(local ([define o ,test-obj])
          (send o fail-method 12)))
          "method not found")

;; Test combinado -> Usando objetos en campos, llamando metodos de los objetos de los campos
;; Se crean dos objetos (o1 o2) iguales al inicio, que tienen un 2 campos con objetos
;; Se envía a los objetos dentro de o1 y o2 (field object1) un set-x
;; En ambos (o1 o2) se cambia un objeto (field num) por un número.
;; Se calcula (- (send o1 multi-num>sum-obj 2) (send o2 multi-num>sum-obj -6))
;; 1.- (send o1 multi-num>sum-obj 2)  -> (* 50 (send (get object1) sum 2))
;;                                    -> (* 50 (+ -7 (+ 2 2))) -> -150
;; 2.- (send o2 multi-num>sum-obj -6) -> (* 32 (send (get object1) sum -6))
;;                                    -> (* 32 (+ -2 (+ 2 -6))) -> -192
;; 3.- (- -150 -192)
(define test-obj1
  `(object
    (field object1 ,test-obj)
    (field num ,test-obj)
    (method multi-num>sum-obj (val) (* (get num) (send (get object1) sum val)))
    (method set-num (val) (set num val))
    (method set-x-obj1 (val) (send (get object1) set-x val))
    (method get-x-obj1 () (send (get object1) get-x))))

(test
 (run-val
  `(local ([define o1 ,test-obj1]
           [define o2 ,test-obj1])
     (seqn
      (send o1 set-x-obj1 -7)
      (seqn
       (send o2 set-x-obj1 -2)
       (seqn
        (send o1 set-num 50)
        (seqn
         (send o2 set-num 32)
         (- (send o1 multi-num>sum-obj 2) (send o2 multi-num>sum-obj -6))))))))
     42)


;; Objeto de prueba sin campos
(define test-obj2
  `(object
    (method multi-num>sum-obj (val) (* (get num) (send (get object1) sum val)))
    (method set-num (val) (set num val))
    (method set-x-obj1 (val) (send (get object1) set-x val))
    (method get-x () (get x))))

;; Set en un objeto sin campos
(test/exn (run-val `(local ([define o ,test-obj2])
              (send o set-num 10)))
          "field not found")

;; Get en un objeto sin campos
(test/exn (run-val `(local ([define o ,test-obj2])
              (send o get-x)))
          "field not found")


;; Test crear campos con valores de otros campos
(test (run-val
       `(local
          ([define o (object
                      (field x 10)
                      (field y (get x))
                      (method get-y () (get y)))])
          (send o get-y))) 10)

;; Test se puede llamar a métodos en los campos
(test (run-val
       `(local
          ([define o (object
                      (field x 10)
                      (method initializate () (+ 5 11))
                      (field y (send this initializate))
                      (method get-y () (get y)))])
          (send o get-y))) 16)

;; Objeto creado desde otro objeto toma el "y" de si mismo
(test (run-val
       `(local
          ([define o (object
                      (field x 10)
                      (method initializate () 3)
                      (field y (send this initializate))
                      (method createb () (object
                                          (field y 99)
                                          (field x (get y))
                                          (method get-x () (get x)))))])
          (send (send o createb) get-x))) 99)

;; Objeto creado desde otro objeto toma el "y" del que lo crea ya que no tiene uno propio
(test (run-val
       `(local
          ([define o (object
                      (field x 10)
                      (method initializate () 6)
                      (field y (send this initializate))
                      (method createb () (object
                                          (field x 4)
                                          (method get-y () (get y)))))])
          (send (send o createb) get-y))) 6)


;; Parte 2
;; Se crean objetos que delegan entre si siguiendo:
;; objeto-metal -> vehiculo -> auto
;;                          -> moto

(define objeto-metal
  '(object
    (field porcentaje-metal 70)
    (field densidad-metal 60)
    (field volumen 10)
    (method get-peso () (* (get densidad-metal) (send this get-volumen)))
    (method get-densidad () (get densidad-metal))
    (method set-densidad (val) (set densidad-metal val))))
    
(define vehiculo
  '(object : objeto-metal
    (field velocidad-actual 0)
    (field velocidad-maxima 100)
    (field volumen 60)
    (method get-va () (get velocidad-actual))
    (method get-velocidad-maxima () (get velocidad-maxima))
    (method get-volumen () (get volumen))
    (method set-vm (val) (set velocidad-maxima val))))
    
(define auto
  '(object : vehiculo
    (field ruedas 4)
    (field velocidad-actual 92)
    (field volumen 900)
    (method get-va () (get velocidad-actual))
    (method diferencia-velocidad () (- (send this get-velocidad-maxima) (get velocidad-actual)))
    (method get-volumen () (get volumen))))

(define moto 
  '(object : vehiculo
    (field ruedas 2)
    (field velocidad-actual 70)
    (field volumen 700)
    (method get-va () (get velocidad-actual))
    (method diferencia-velocidad () (- (send this get-velocidad-maxima) (get velocidad-actual)))
    (method get-volumen () (get volumen))))

;; Auto no posee método velocidad máxima, se consulta delegando
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (send auto get-velocidad-maxima)))
          100)

;; Auto no posee densidad, lo debe delegar, a quien delega tampoco lo tiene, debe delegar (delegar más de una vez)
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (send auto get-densidad)))
          60)

;; Ningún objeto posee un método
(test/exn (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (send auto get-combustible)))
          "method not found")

;; Usando mismo método con delegación
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (- (send auto get-velocidad-maxima) (send moto get-velocidad-maxima))))
          0)

;; Usando mismo método con delegación pero cambiando el valor a mitad de la ejecución
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (- (send auto get-velocidad-maxima)
             (seqn
              (send vehiculo set-vm 1)
              (send moto get-velocidad-maxima)))))
          99)

;; Obtener diferencia de velocidad (pasar this en la delegacion)
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (+ (send auto get-peso) (send moto get-peso))))
          96000)

;; Usando auto y moto para obtener su peso con los métodos de objeto-metal (pasando this en la delegación)
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (+ (send auto diferencia-velocidad) (send moto diferencia-velocidad))))
          38)

;; Delegación, en la que el objeto al que se le delega no existe
(test/exn (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : no-existe))]
               (send oracle secret? everything)))
      "free identifier")

;; Set de valores con delegación, se hace set de la densidad desde el auto
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (+ (send auto get-densidad)
             (seqn
              (send auto set-densidad 1)
              (send auto get-densidad)))))
          61) 

;;; Parte 3

;; Shallow copy y Deep copy entregan igual resultado en objeto sin delegacion
(test
 (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
 6)
(test
 (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
 6)

;; Test Shallow Copy
;; Se hace una copia de auto y la copia hace set al valor de un objeto al que se debe delegar (objeto-metal)
;; Este cambio afecta al (objeto-metal que comparten)
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (seqn
           (send auto set-densidad 1)
           (local ([define auto-2 (shallow-copy auto)])
             (+ (send auto-2 get-densidad)
                (seqn
                 (send auto-2 set-densidad 10)
                 (+ (send auto get-densidad) (send auto-2 get-densidad))))))))
      21)

;; Test Deep Copy
;; Se hace una copia de auto y la copia hace set al valor de un objeto al que se debe delegar (objeto-metal)
;; Este cambio no afecta al (objeto-metal que comparten), ya que crea otro
(test (run-val
       `(local ([define objeto-metal ,objeto-metal]
                [define vehiculo ,vehiculo]
                [define auto ,auto]
                [define moto ,moto])
          (seqn
           (send auto set-densidad 1)
           (local ([define auto-2 (deep-copy auto)])
             (+ (send auto-2 get-densidad)
                (seqn
                 (send auto-2 set-densidad 10)
                 (+ (send auto get-densidad) (send auto-2 get-densidad))))))))
      12)
                   
;; Test usando objeto del field, dan iguales ya que duplica los campos
;; Shallow Copy
(test (run-val
       `(local ([define obj1 ,test-obj1])
          (seqn
           (send obj1 set-x-obj1 9)
           (+ (send obj1 get-x-obj1)  ; 9
              (local ([define obj2 (shallow-copy obj1)])
                (+ (send obj2 get-x-obj1) ;9
                   (seqn
                    (send obj2 set-x-obj1 20)
                    (+ (send obj2 get-x-obj1) (send obj1 get-x-obj1))))))))) ; 40
      58)
;; Deep Copy
(test (run-val
       `(local ([define obj1 ,test-obj1])
          (seqn
           (send obj1 set-x-obj1 9)
           (+ (send obj1 get-x-obj1)  ; 9
              (local ([define obj2 (deep-copy obj1)])
                (+ (send obj2 get-x-obj1) ;9
                   (seqn
                    (send obj2 set-x-obj1 20)
                    (+ (send obj2 get-x-obj1) (send obj1 get-x-obj1))))))))) ; 40
      58)


                 
;; Test Bonus
;; Ejemplo
(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5)))
      10)

;; Usandola como lamba
(test (run-val
       '((fun (x)(+ x x)) 5))
      10)

;; Función sobre objetos
(test (run-val
       `(local
          ([define o ,test-obj]
          [define o2 ,test-obj])
          ((fun (x) (send x sum 10)) o)))
      13)

;; Función sobre dos objetos
(test (run-val
       `(local
          ([define o1 ,test-obj]
           [define o2 (shallow-copy o1)])
          ((fun (x y) (+ (send x sum 10) (send x sum 10))) o1 o2)))
      26)




