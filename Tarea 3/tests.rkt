#lang play
;; Tarea de Rodrigo Urrea
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
                (method foo () 5)
                ))
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

; test Objetos (P1)
(module+ test
  ;; creacion normal de un objeto
  (test (run-val
         `(local ([define o (object (field x 1)
                                    (method sum-x (v) (+ (get x) v)))])
            (send o sum-x (+ 1 2))))
        4)
  ;; objeto sin campos y metodos que se llaman entre ellos
  (test (run-val
         `(local ([define o (object (method get-x () 10)
                                    (method ltn-x (n) (< n (send this get-x))))])
            (send o ltn-x (+ 1 2))))
        #t)
  ;; objeto sin metodos, como los objetos tienen encapsulacion fuerte no es
  ;; posible acceder a su informacion fuera de ellos
  (test (run-val
         `(local ([define o (object (field x (+ 5 6))
                                    (field y (* 3 (get x))))])
            10))
        10)
  ;; objeto dentro de un objeto
  (test (run-val
         `(local ([define o (object (field x (+ 5 6))
                                    (field y (- 3 4))
                                    (method cuadrant-3? () (and (< 0 (get x))
                                                                (< 0 (get y)))))]
                  [define p (object (field obj o)
                                    (method get-obj () (get obj)))])
            (send (send p get-obj) cuadrant-3?)))
        #f)
  ;; llamar this fuera de obj
  (test/exn (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            (seqn
             (send o get-x)
             this)))
        "this used outside of an object")
  ;; llamar set fuera de obj
  (test/exn (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            (seqn
             (send o get-x)
             (set x 20))))
        "set used outside of an object")
  ;; llamar get fuera de obj
  (test/exn (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            (seqn
             (send o get-x)
             (get x))))
        "get used outside of an object")
  ;; se llama al obj al cual se le envia el mensaje y no otro
  (test (run-val
         `(local ([define o (object (field x (+ 5 6))
                                    (method get-x () (get x)))]
                  [define p (object (field x 3)
                                    (method get-x () (get x)))])
            (send o get-x)))
        11)
  (test (run-val
         `(local ([define o (object (field x (+ 5 6))
                                    (method get-x () (get x)))]
                  [define p (object (field x 3)
                                    (method get-x () (get x)))])
            (send p get-x)))
        3)
  ;; si se llama a un campo o metodo no existente se produce error
  (test/exn (run-val
             `(local ([define o (object (field x 10)
                                        (method get () (get y)))])
                (send o get)))
            "field not found")
  (test/exn (run-val
             `(local ([define o (object (field x 10)
                                        (method get (fd) (get fd)))])
                (send o set-x 10)))
            "method not found")
  ;; los miembros de un obj se ejecutan en orden y puden interactuar entre ellos
  (test (run-val
             `(local ([define val (+ 10 20)]
                      [define o (object (field x val)
                                        (field y (* (get x) val))
                                        (method sum-x-y () (+ (get x) (get y))))])
                (send o sum-x-y)))
            930)
  ;; se pueden definir objetos en objetos
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method create-obj () (object (method get-x () (get x)))))])
            (send (send o create-obj) get-x)))
        10)

  ;; se pueden modificar los campos de un objeto
  (test (run-val
         `(local ([define o (object (field x 5)
                                    (method set-x (val) (set x val))
                                    (method get-x () (get x)))])
            (seqn
             (send o set-x (+ 1 2))
             (send o get-x))))
        3)

  ;; los objetos son valores, por lo que se pueden retornar
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            o))
        (list 'object (list 'x) (list 'get-x) "null"))
  )
; test Delegacion (P2)
(module+ test
  ;; se le puede dar delegacion a objetos
  (test (run-val
         `(local ([define o (object (method foo () 42))]
                  [define p (object : o)])
            (send p foo)))
        42)
  ;; si no hay delegacion hay error
  (test/exn (run-val
         `(local ([define o (object (method foo () 42))]
                  [define p (object)])
            (send p foo)))
        "method not found")
  ;; this es el objeto llamado, no el delegado
  (test (run-val
         `(local ([define o (object (method foo () 42)
                                    (method app-foo () (send this foo)))]
                  [define p (object : o (method foo () 20))])
            (send p app-foo)))
        20)

  ;; solo existe delegacion de metodos, no de campos
  (test/exn (run-val
             `(local ([define o (object (field x 10))]
                      [define p (object : o (method get-x () (get x)))])
                (send p get-x)))
            "field not found")

  ;; existe delegacion encadenada
  (test (run-val
         `(local ([define o (object (method foo () 10))]
                  [define p (object : o)]
                  [define q (object : p)])
            (send q foo)))
        10)

  ;; pero this sigue siendo el objeto al cual es usuario llamo
  (test (run-val
         `(local ([define o (object (method app-foo () (send this foo))
                                    (method foo () 3))]
                  [define p (object : o (method foo () 2))]
                  [define q (object : p (method foo () 1))])
            (send q app-foo)))
        1)
  ;; se puede mutar los campos en los llamados por delegacion
  ;; pero se afecta al objeto de donde se llama
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x))
                                    (method set-x (val) (set x val)))]
                  [define p (object : o
                                    (field x 20)
                                    (method get-x () (get x)))])
            (seqn
             (send p set-x (+ 2 3))
             (send p get-x))))
        20)
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x))
                                    (method set-x (val) (set x val)))]
                  [define p (object : o
                                    (field x 20)
                                    (method get-x () (get x)))])
            (seqn
             (send p set-x (+ 2 3))
             (send o get-x))))
        5)
  ;; si 2 objetos delegan al mismo y lo mutan, se aplican ambas mutaciones
  (test (run-val
         `(local ([define o (object (field x 0)
                                    (method increment () (set x (+ 1 (get x))))
                                    (method get-x () (get x)))]
                  [define p (object : o)]
                  [define q (object : o)])
            (seqn
             (send p increment)
             (seqn
              (send q increment)
              (send o get-x)))))
        2)
  )

; test copias (P3)
(module+ test
  ;; shallow-copy retorna una copia del objecto
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            (shallow-copy o)))
  (list 'object (list 'x) (list 'get-x) "null"))
  ;; es una copia por lo cual modificar uno no influye en el otro
  (test (run-val
        `(local ([define o (object (field x 1)
                                   (method set-x (val) (set x val))
                                   (method get-x () (get x)))]
                 [define p (shallow-copy o)])
           (seqn
            (send p set-x 10)
            (send o get-x))))
        1)
  (test (run-val
        `(local ([define o (object (field x 1)
                                   (method set-x (val) (set x val))
                                   (method get-x () (get x)))]
                 [define p (shallow-copy o)])
           (seqn
            (send o set-x 10)
            (send p get-x))))
        1)
  ;; shallow-copy utiliza el mismo parent en ambos objetos
  (test (run-val
         `(local ([define o (object (field x 1)
                                    (method inc () (set x (+ 1 (get x))))
                                    (method get-x () (get x)))]
                  [define p (object : o)]
                  [define q (shallow-copy p)])
            (seqn
             (send p inc)
             (seqn
              (send q inc)
              (send o get-x)))))
        3)
  ;; deep-copy copia un objeto
  (test (run-val
         `(local ([define o (object (field x 10)
                                    (method get-x () (get x)))])
            (deep-copy o)))
  (list 'object (list 'x) (list 'get-x) "null"))
  ;; las copias no influyen una en la otra
  (test (run-val
        `(local ([define o (object (field x 1)
                                   (method set-x (val) (set x val))
                                   (method get-x () (get x)))]
                 [define p (deep-copy o)])
           (seqn
            (send p set-x 10)
            (send o get-x))))
        1)
  (test (run-val
        `(local ([define o (object (field x 1)
                                   (method set-x (val) (set x val))
                                   (method get-x () (get x)))]
                 [define p (deep-copy o)])
           (seqn
            (send o set-x 10)
            (send p get-x))))
        1)
  ;; deep-copy copia recursivamente los parents
  (test (run-val
         `(local ([define o (object (field x 1)
                                    (method inc () (set x (+ 1 (get x))))
                                    (method get-x () (get x)))]
                  [define p (object : o)]
                  [define q (deep-copy p)])
            (seqn
             (send p inc)
             (seqn
              (send q inc)
              (send o get-x)))))
        2)
  ;; aqui aumenta 2 veces porque o esta asociado a p
  (test (run-val
         `(local ([define o (object (field x 1)
                                    (method inc () (set x (+ 1 (get x))))
                                    (method get-x () (get x)))]
                  [define p (object : o)]
                  [define q (deep-copy p)])
            (seqn
             (send p inc)
             (seqn
              (send p inc)
              (send o get-x)))))
        3)
  ;; aqui no aumenta porque o no esta asociado a q, si no a una copia de el
  (test (run-val
         `(local ([define o (object (field x 1)
                                    (method inc () (set x (+ 1 (get x))))
                                    (method get-x () (get x)))]
                  [define p (object : o)]
                  [define q (deep-copy p)])
            (seqn
             (send q inc)
             (seqn
              (send q inc)
              (send o get-x)))))
        1)
  )
; test funciones con objetos (P4)
(module+ test
  ;; nuestro lenguaje ahora soporta funciones, que son un azucar sintactico para
  ;; referirse a objetos con un unico metodo, en nuestro caso apply
  (test (run-val
         `(local ([define double (fun (x) (+ x x))])
            (double 10)))
        20)
  ;; las funciones tienen scope lexico
  (test (run-val
         `(local ([define n 10]
                  [define addn (fun (x) (+ x n))])
            (local ([define n 20])
              (addn 5))))
        15)
  )