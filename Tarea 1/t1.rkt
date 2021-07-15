#lang play
(print-only-errors #t)

#|
<expr>  ::= {<num>}
        | <id>
        | <bool>
        | {<unop> <expr>}
        | {<binop> <expr> <expr>}
        | {if <expr> <expr> <expr>}
        | {with {{<id> <expr>}*} <expr>}
        | {<id> <expr>*}
|#
(deftype Expr
  [num n]
  [id name]
  [bool b]
  [unop op1 c]
  [binop op2 l r]
  [if-new v t f]
  [with defs body]
  [app name es])

; not-pro: funcion not con check dinamico de tipos
(define (not-pro x) (if (boolean? x)
                        (not x)
                        (error 'not-pro "Runtime type error:\n expected Boolean in ~a" x)))

; not-pro: funcion add1 con check dinamico de tipos
(define (add1-pro x) (if (number? x)
                        (add1 x)
                        (error 'add1-pro "Runtime type error:\n expected Number in ~a" x)))

; not-pro: funcion sub1 con check dinamico de tipos
(define (sub1-pro x) (if (number? x)
                        (sub1 x)
                        (error 'sub1-pro "Runtime type error:\n expected Number in ~a" x)))

;<unop>  ::= ! | add1 | sub1
; lista de operadores unarios permitidos
(define unops (list '! 'add1 'sub1))

; is-unop?: Sym -> Bool
; recibe un simbolo e indica si dicho simbolo corresponde a un op unario
; de los definidos en la lista unops, retornando en dicho caso una lista 
; con el operador en su cabeza 
(define (is-unop? x) (member x unops))

; parse-unop: Sym -> <procedure> o Error
; recibe un simbolo y entrega el unop asociado o un Parse error
(define (parse-unop un)
  (match un
    ['! not-pro]
    ['add1 add1-pro]
    ['sub1 sub1-pro]
    [a (error 'parse-unop "Parse error:\n ~a is not an unop" un)]))

; sum-pro: funcion + con check dinamico de tipos
(define (sum-pro x y) (if (and (number? x) (number? y))
                          (+ x y)
                          (error 'sum-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; res-pro: funcion - con check dinamico de tipos
(define (res-pro x y) (if (and (number? x) (number? y))
                          (- x y)
                          (error 'res-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; mul-pro: funcion * con check dinamico de tipos
(define (mul-pro x y) (if (and (number? x) (number? y))
                          (* x y)
                          (error 'mul-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; div-pro: funcion / con check dinamico de tipos
(define (div-pro x y) (if (and (number? x) (number? y))
                          (/ x y)
                          (error 'div-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; and-pro: funcion and con check dinamico de tipos y como procedimiento
(define (and-pro x y) (if (and (boolean? x) (boolean? y))
                          (and x y)
                          (error 'and-pro "Runtime type error:\n expected Boolean in ~a and ~a" x y)))

; eq-pro: funcion = con check dinamico de tipos
(define (eq-pro x y) (if (and (number? x) (number? y))
                          (= x y)
                          (error 'eq-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; lt-pro: funcion < con check dinamico de tipos
(define (lt-pro x y) (if (and (number? x) (number? y))
                          (< x y)
                          (error 'lt-pro "Runtime type error:\n expected Number in ~a and ~a" x y)))

; <binop>  ::= + | - | * | / | && | = | < | ...
; lista de operadores binarios permitidos
(define binops (list '+ '- '* '/ '&& '= '<))

; is-binop?: Sym -> Bool
; recibe un simbolo e indica si dicho simbolo corresponde a un op binario
; de los definidos en la lista binops
(define (is-binop? x) (member x binops))

; parse-binop: Sym -> <procedure> o Error
; recibe un simbolo y entrega el binop asociado o un Parse error
(define (parse-binop bin)
  (match bin
    ['+ sum-pro]
    ['- res-pro]
    ['* mul-pro]
    ['/ div-pro]
    ['&& and-pro]
    ['= eq-pro]
    ['< lt-pro]
    [a (error 'parse-binop "Parse error:\n ~a is not a binop" bin)]))

; parse: Src -> Expr o Error
; Convierte sintaxis concreta (del programa principal o
;body fundefs) en sintaxis abstracta o Parse error
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list (? number? n)) (num n)]
    [(list (? boolean? n)) (bool n)]
    [(list (? is-unop? op) s1) (unop (parse-unop op) (parse s1))]
    [(list (? is-binop? op) l r) (binop (parse-binop op) (parse l) (parse r))]
    [(list 'if val tr fa) (if-new (parse val) (parse tr) (parse fa))]
    [(list 'with (list rest ...) body) (with (parse-defs rest) (parse body))]
    [(list name exprs ...) (app name (parse-exprs exprs))]
    [rest (error 'parse "Parse error:\n the expression ~a not parse" src)]
    ))

; parse-defs: (Listof (Listof Src Src)) -> (Listof (Listof Sym Expr))* o Error
; en P2 * es (Listod (Listof Sym Type Expr))
; Convierte la lista de definiciones provenientes del parseo de with en
; otra lista pero con las expresiones en sintaxis abstracta o Parse
; error
(define (parse-defs defs)
  (match defs
    ['() '()]
    [(list (list name : type val) rest ...) (cons (list name (parse-type type) (parse val)) (parse-defs rest))]
    [(list (list name val) rest ...) (cons (list name (Any) (parse val)) (parse-defs rest))]
    [defs (error 'parse-defs "Parse error:\n in with you must put name val or name : type val")]))

; parse-exprs: (Listof Src) -> (Listof Expr)
; Convierte la lista de expresiones provenientes del parseo de un aplicacion
; (ultimo caso de parse) en otra lista con las expresiones en sintaxis abstracta
(define (parse-exprs expr)
  (match expr
    ['() '()]
    [(list e rest ...)  (cons (parse e) (parse-exprs rest))]))

; <env> ::= (mtEnv)
;         | (aEnv <id> <val> <env>)
(deftype Env
  [mtEnv]
  [aEnv id val env])

; empty-env: ambiente base
(define empty-env  (mtEnv))

; extend-env: Sym Expr Env -> Env
; Recibe el nombre de una variable, su valor y un ambiente y retorna
; un nuevo ambiente que contiene la nueva variable y todas las definidas
; anteriormente
(define extend-env aEnv)

; extend-env-list: (Listof Sym Type Expr) (Listof FunDef) Env Env -> Env
; Extiende el segundo ambiente con una lista de definiciones provenientes
; de la interpretacion de un with, para ello se interpretan los valores
; con el primer ambiente (antiguo) y se agregan dichos valores con su symbolo
; al segundo ambiente (nuevo)
(define (extend-env-list defs fundef old-env new-env)
  (match defs
    ['() new-env]
    [(list (list name _ val) rest ...) (extend-env-list rest fundef old-env
                                               (extend-env name (interp val fundef old-env) new-env)) ]))

; extend-env-type: (Listof Sym Type Expr) Env -> Env
; Extiende el ambiente dado una lista de argumentos de funcion, se
; utiliza para el typecheck, en esta funcion solo interesa el tipo
; de las expresiones que se guardan en el ambiente, no su valor.
(define (extend-env-type defs env)
  (match defs
    ['() env]
    [(list (list name type) rest ...) (extend-env-type rest
                                               (extend-env name type env)) ]
    [(list (list name type _) rest ...) (extend-env-type rest
                                               (extend-env name type env)) ]))

; env-lookup: Sym Env -> Expr o Error
; Recibe un simbolo y un ambiente y retorna el valor asociado a ese simbolo o
; un error* en caso de no encontrar el simbolo
; En P2 * se transforma en error estatico, antes era dinamico
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "Static error: free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (eq? id x)
         val
         (env-lookup x rest))]))

; <fundef>  ::= {define {<id> <id>*} <expr>}
(deftype FunDef
  [fundef fname args type body])

; parse-fundef : Src -> FunDef o error
; Convierte la sintaxis concreta de una definicion de funcion en
; sintaxis abstracta, generando una FunDef o un Parse error.
(define (parse-fundef src)
  (match src
  [(list 'define {list fname args ...} body) (fundef fname (add-type args) (Any) (parse body))]
  [(list 'define {list fname args ...} : type body) (fundef fname (add-type args) (parse-type type) (parse body))]
  [rest (error 'parse-fundef "Parse error:\n can not parse ~a" src)]))

; lookup-fundef: Id List[FunDef] -> FunDef o error
; Busca el id de una funcion en la lista de fundef, retornandola
; en caso de encontrarlo, o en caso contrario levantando un
; error *
; en P2 * se transforma en un error estatico
(define (lookup-fundef fname fundefs)
  (match fundefs
    ['() (error "Static error: undefined function:" fname)]
    [(cons fd fds) (if (eq? (fundef-fname fd) fname)
                       fd
                       (lookup-fundef fname fds))]))

; zip: (Listof id) (Listof Expr) -> (Listof id Expr) o error
; Junta las 2 listas para generar una solo con pares, o un Runtime
; error en caso de que las listas tengan distinto largo
(define (zip args vals)
  (match args
    ['() (match vals
           ['() '()]
           [(list _ rest ...) (error 'zip "Runtime error:\n Arity mismatch, vals list longer than args list")])]
    [(list (list arg type _) arg-rest ...) (match vals
                           ['() (error 'zip "Runtime error:\n Arity mismatch, args list shorter than vals list") ]
                           [(list val val-rest ...) (cons (list arg type val) (zip arg-rest val-rest))])]))

; interp :: Expr (Listof FunDef) Env -> Val (Number o Boolean) o Error
; Funcion que interpreta el arbol de sintaxis abstracta para generar
; con ello un valor (booleano o numero) o un Runtime error
(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)]
    [(unop op c) (op (interp c fundefs env))]
    [(binop op l r) (op (interp l fundefs env) (interp r fundefs env) )]
    [(if-new val tr fa) (define value (interp val fundefs env))
                        (if (boolean? value)
                         (if (interp val fundefs env)
                             (interp tr fundefs env)
                             (interp fa fundefs env))
                         (error 'interp "Runtime type error:\n expected Boolean found num in if"))]
    [(with defs b)
     (interp b fundefs (extend-env-list defs fundefs env env))]
    [(app f es)
     (def (fundef name args _ body) (lookup-fundef f fundefs))
     (if (eq? name 'true-cont)
         #t
         (begin (check-conts args es fundefs env)
         (interp body fundefs (extend-env-list (zip args es) fundefs env empty-env))))]))

; bool-fun? : Binop|Unop -> Bool
; Funcion que indica si la operacion se puede hacer entre
; operadores Bool o no
(define (bool-fun? op)
  (or (eq? op not) (eq? op and-pro)))

; num-fun? : Binop|Unop -> Bool
; Funcion que indica si la operacion se puede hacer entre
; operadores Num o no
(define (num-fun? op)
  (and (not (eq? op not)) (not (eq? op and-pro)) (not (eq? op =))))

; <prog>  ::= {<fundef>* <expr>}

; run : <prog> [Boolean] -> Val (Number o Boolean) o Error
; Funcion que ejecuta un programa, no usa typecheck solo si se
; ingresa un #f como argumento (para tener Runtime error en
; operadores)
(define (run lines [check #t])
  (if check
      (begin (typecheck lines) (run-after lines))
      (run-after lines)))

; run-after: <prog> (Listof FunDef)-> Val (Number o Boolean) o Error
; Antigua funcion run, solo ejecuta el codigo, necesita una
; lista de fundefs para P3, ya que se necesita una FunDef del
; contrato default que siempre retorna true, ademas de tener
; que guardar las nuevas FunDef que se definen
(define (run-after lines [fundefs base-fundef])
  (match lines
    [(list line rest ...)
     (if (eq? (car line) 'define)
         (run-after rest (cons (parse-fundef line) fundefs))
         (interp (parse (car lines)) fundefs empty-env))]))

; <type> ::= Num|Bool|Any
(deftype Type
  [Num]
  [Bool]
  [Any])

; parse-type : Sym -> Type or Error
; Lleva un simbolo al tipo que representa o Parse erorr en caso
; de no sea valido el simbolo
(define (parse-type type)
  (if (eq? type 'Num)
             (Num)
             (if (eq? type 'Bool)
                 (Bool)
                 (if (eq? type 'Any)
                     (Any)
                     (error 'parse-type "Parse error:\n ~a is not a type" type)))))

; parse-sym-type : Type -> Sym or Error
; Dado un type retorna un symbolo asociado o Parse error (no
; deberia fallar esta funcion por como esta construido el
; lenguaje)
(define (parse-sym-type type)
  (if (Any? type)
      'Any
      (if (Num? type)
          'Num
          (if (Bool? type)
              'Bool
              (error 'parse-sym-type "Parse error: ~a is not a type" type)))))
  
; add-type : (Listof Sym [<Type>] [Sym]) -> (Listof Sym <Type>) o Error
; Toma una lista con simbolos, algunos tipos (pueden faltar) y
; algunos contratos* (pueden faltar) y la completa con Anys y
; funciones que retornar true para que cada simbolo tenga un Type y
; contrato. Retorna Parse error en caso de que se use simbolo extraño
; tanto para Type como contratos.
; Para P3 se agregaro *
(define (add-type args)
  (match args
    ['() '()]
    [(list (list name _ type _ cont) rest ...)
     (cons (list name (parse-type type) cont) (add-type rest))]
    [(list (list name sym ex) rest ...)
     (if (eq? sym '@)
         (cons (list name (Any) ex) (add-type rest))
         (if (eq? sym ':)
             (cons (list name (parse-type ex) 'true-cont) (add-type rest))
             (error 'parse-defs "Parse error:\n Symbol ~a is not permited" sym)))]
    [(list name rest ...)
     (cons (list name (Any) 'true-cont) (add-type rest))]))

; obtain-fundefs: Prog -> (Listof FunDef)
; obtiene la lista de FunDefs de un programa, util para
; el typecheck
(define (obtain-fundefs prog)
  (if (eq? (length prog) 1)
      base-fundef
      (match prog
        [(list line rest ...)
         (if (eq? (car line) 'define)
             (cons (parse-fundef line) (obtain-fundefs rest))
             base-fundef)])))

; comp-type: Type Type -> Boolean
; Compara los 2 tipos y determina si son compatibles o no, usando
; la regla de que siempre hay compatibilidad si almenos uno es Any.
(define (comp-type obt exp)
  (if (or (Any? obt) (Any? exp))
      #t
      (if (or (and (Bool? obt) (Bool? exp)) (and (Num? obt) (Num? exp)))
          #t
          #f)))

; check-list-type: (Listof Sym Type Expr) (Listof FunDef) Env -> Boolean o Error
; Verifica que los tipos de las variables declaradas en un with coincidan
; con sus valores asociados, si es asi retorna #t, si no retorna
; Static type error.
(define (check-list-type args fundefs env)
  (match args
    ['() #t]
    [(list (list _ type expr) rest ...)
     (define expr-type (typecheck-main expr fundefs env))
     (if (comp-type expr-type type)
         (check-list-type rest fundefs env)
         (error 'typecheck "Static type error: expected ~a found ~a in with variables"
                (parse-sym-type type) (parse-sym-type expr-type)))]))

; comp-type-list: (Listof Expr) (Listof Sym Type) (Listof FunDef) Env Sym Type -> Type o Error
; Verfica que los tipos entregados como argumento a una aplicacion de
; funcion coincidan con los que se declararon al definirla, en caso que
; asi sea de retorna el tipo de la funcion, si no un Static error.
(define (comp-type-list es args fundefs env fname type)
  (match es
    ['() (match args
           ['() type]
           [args (error 'typecheck "Static error: the function ~a requires more arguments" fname)])]
    [(list e es-rest ...)
     (match args
       ['() (error 'typecheck "Static error: the function ~a requires less arguments" fname)]
       [(list (list _ arg-type cont) arg-rest ...)
        (define e-type (typecheck-main e fundefs env))
        (val-cont cont fundefs)
        (if (comp-type e-type arg-type)
            (comp-type-list es-rest arg-rest fundefs env fname type)
            (error 'typecheck "Static type error: expected ~a found ~a in ~a"
                   (parse-sym-type arg-type) (parse-sym-type e-type) fname))])]))

; type-map: Binop|Unop -> Type
; Entrega el tipo de input que acepta un operador dado
(define (type-map op)
  (if (or (eq? not-pro op) (eq? and-pro op))
      (Bool)
      (Num)))

; op-type: Binop|Unop -> Type
; Entrega el tipo de output que entrega un operador dado
(define (op-type op)
  (if (or (eq? not-pro op) (eq? and-pro op) (eq? eq-pro op) (eq? lt-pro op))
      (Bool)
      (Num)))

; typecheck: Src -> Type o Error
; Recibe un programa y verifica que los tipos definidos sean correctos
; y entrega el tipo de la expresion revisada o un Static error.
(define (typecheck prog)
  (parse-sym-type (typecheck-prog prog)))

; typecheck-prog: Src (Listof FunDef) -> Type o Error
; typecheck pero que permite guardar fundefs para no calcularlas
; en cada paso
 (define (typecheck-prog prog [fundefs '()])
   
   (define funs (if (eq? fundefs '())
                    (obtain-fundefs prog)
                    fundefs))
   (if (eq? (length prog) 1)
       (typecheck-main (parse (car prog)) funs empty-env)
       (match prog
         [(list line rest ...)
          (if (eq? (car line) 'define)
              (begin (typecheck-fundef (parse-fundef line) funs) (typecheck-prog rest funs))
              (typecheck-main (parse line) funs empty-env))])))
  
; typecheck-fundef: Src (Listof FunDef) -> Type o Error
; Verifica que las funciones que se definen en el programa
; sean validas o retorna un Static error.
(define (typecheck-fundef def fundefs)
  (match def
    [(fundef fname args type body)
     (define body-type (typecheck-main body fundefs (extend-env-type args empty-env)))
     (if (comp-type body-type type)
         type
         (error 'typecheck "Static type error: expected ~a found ~a in fundef ~a"
                (parse-sym-type type) (parse-sym-type body-type) fname))]))

; typecheck-main: Src (Listof FunDef) Env -> Type o Error
; Verifica que el programa principal o los cuerpos de las funciones
; sean validos, si no retorna un Static error.
(define (typecheck-main prog fundefs env)
  (match prog
    [(num n) (Num)]
    [(bool b) (Bool)]
    [(id x) (env-lookup x env)]
    [(unop op c)
     (define c-type (typecheck-main c fundefs env))
     (define op-type-input (type-map op))
     (if (comp-type c-type op-type-input)
         (op-type op)
         (error 'typecheck "Static type error: expected ~a found ~a in ~a"
                (parse-sym-type op-type-input) (parse-sym-type c-type) op))]
    [(binop op l r)
     (define l-type (typecheck-main l fundefs env))
     (define r-type (typecheck-main r fundefs env))
     (define op-type-input (type-map op))
     (if (and (comp-type l-type op-type-input) (comp-type r-type op-type-input))
         (op-type op)
         (error 'typecheck "Static type error: expected ~a found ~a and ~a in ~a"
                (parse-sym-type op-type-input) (parse-sym-type l-type) (parse-sym-type r-type) op))]
    [(if-new val tr fa)
     (define val-type (typecheck-main val fundefs env))
     (define tr-type (typecheck-main tr fundefs env))
     (define fa-type (typecheck-main fa fundefs env))
     (if (comp-type val-type (Bool))
         (if (comp-type tr-type fa-type)
             tr-type
             (error 'typecheck "Static type error: expected both branches Bool or Num found ~a and ~a in if"
                    (parse-sym-type tr-type) (parse-sym-type fa-type)))
         (error 'typecheck "Static type error: expected Bool found ~a in if"
                (parse-sym-type val-type)))]
    [(with defs body)
     (begin (check-list-type defs fundefs env)
     (typecheck-main body fundefs (extend-env-type defs env)))]
    [(app f es)
     (def (fundef name args type _) (lookup-fundef f fundefs))
     (comp-type-list es args fundefs env name type)]))

; base-fundef ;== (Listof FunDef)
; Lista de fundefs basicas del lenguaje, tiene la definicion de la
; funcion true-cont, la cual se usara como contrato por defecto de
; funciones.
(define base-fundef (cons (parse-fundef '{define {true-cont {x : Any}} : Bool #t}) '()))

; val-cont : <procedure> (Listof FunDef) -> Boolean o Error
; Funcion que valida la correcta definicion de un contrato, es
; decir que reciba solo 1 argumento y que retorne un Bool o Any,
; si no retorna un Static contract error.
(define (val-cont cont fundefs)
  (def (fundef name args type _) (lookup-fundef cont fundefs))
  (if (or (not (Bool? type)) (not (eq? (length args) 1)))
      (error 'val-cont "Static contract error: invalid type or args length for ~a"
             name)
      #t))

; check-conts: (Listof Sym Type Sym) (Listof Expr) (Listof FunDef) env -> Boolean o Error
; Verifica que los argumentos de una aplicacion cumplan con los contratos
; definidos para la misma, si no provoca un Runtime error.
(define (check-conts args es fundefs env)
  (match args
    ['() (match es
           ['() #t]
           [es (error 'check-conts "Runtime error:\n Arity mismatch, vals list longer than args list")])]
    [(list (list _ _ cont) rest-args ...)
     (match es
       ['() (error 'check-conts "Runtime error:\n Arity mismatch, vals list shorter than args list")]
       [(list ex rest-es ...)
        (def (fundef cont-name (list (list name _ _)) _ body) (lookup-fundef cont fundefs))
        (if (interp (app cont-name (list ex)) fundefs env)
            (check-conts rest-args rest-es fundefs env)
            (error 'check-conts "Runtime contract error: ~a does not satisfy ~a"
                   (interp ex fundefs env) cont-name))])]))

