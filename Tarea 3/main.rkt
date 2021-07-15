#lang play
;; Tarea de Rodrigo Urrea
#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

;EXTENSION PARA Funciones
<expr>  ::= ... (todo lo anterior)
         | (fun (<id>*) <expr>)
         | (<expr> <expr>*)
|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  ; extension para objetos
  (obj del-obj members) ;; agregamos tambien el objeto al cual se delegara
  (get arg-name)
  (set arg-name expr)
  (send object mname args)
  (this)
  ; agregamos copias de objetos
  (shallow-copy expr)
  (deep-copy expr)
  ; agregamos un null obj, obj que sera creado "a mano"
  (null-obj))

;; values
(deftype Val
  (numV n)
  (boolV b)
  ;; agregamos el valor de los objetos
  (objV field-hash method-hash parent)
  ; se tiene un hash para campos y otro para metodos (porque las funciones no son valores en el lenguaje)
  ; tambien se agrega un campo parent donde va el objeto que se va a delegar
  (voidV)) ; se añade el valor nulo al lenguaje para indicar el fin de la delegacion

;; semi-values
;; se agregan debido a la necesidad de agregar pseudo-clausuras al lenguaje
;; para poder empaquetar mejor la informacion de los metodos de objetos
;; no son valores pues no tienen una forma explicita de ser creados y no
;; pueden ser retornados
(deftype SemiVal
  (mClosure args body env))

(deftype Def
  (my-def id expr))

;; agregamos los miebros de objetos
(deftype Member
  (field-member fname fexpr)
  (method-member mname margs mbody))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))

;; init-env
;; ambiente inicial que presenta unicamente el identificador this asociado
;; al simbolo 'outside-no-obj, se usa para inicializar la variable this (palabra
;; reservada del lenguaje) y asi detectar cuando se esta dentro o fuera de un obj.
(def init-env (multi-extend-env (list 'this) (list 'outside-no-obj) empty-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (if (equal? s-expr 'this)
                     (this) ;; this es un identificador reservado del lenguaje
                     ;; se agrega Expr (this) para poder generar un errores
                     ;; personalizados y no los default para simbolos
                     (id s-expr))]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ;; Se agregan las expresiones asociadas a Objetos
    [(list 'object ': del-obj members ...) ; si ingresan del-obj
     (obj (parse del-obj) (map parse-member members))]
    [(list 'object members ...) ; si no definimos un nulo
     (obj (null-obj) (map parse-member members))]
    [(list 'get fname) (get fname)]
    [(list 'set fname fexpr) (set fname (parse fexpr))]
    [(list 'send expr mname margs ...)
     (send (parse expr) mname (map parse margs))]
    ;; agregamos las expresiones para copiar objetos
    [(list 'shallow-copy expr) (shallow-copy (parse expr))]
    [(list 'deep-copy expr) (deep-copy (parse expr))]
    ;; extension para soportar funciones
    [(list 'fun (list args ...) body)
     ; una funcion la codificamos como un obj con metodo apply
     (obj (null-obj) (list (method-member 'apply args (parse body))))]
    [(list expr args ...)
     ; una aplicacion de funcion la codificamos como un send de apply
     ; a la funcion (objeto)
     (send (parse expr) 'apply (map parse args))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; parse-member :: s-expr -> Member
;; parsea una s-expr proveniente de la definicion de un objeto en un field-member o en un method-member.
(define (parse-member member)
  (match member
    [(list 'field fname fexpr)
     (field-member fname (parse fexpr))]
    [(list 'method mname (list args ...) mbody)
     (method-member mname args (parse mbody))]))

;; interp :: Expr Env Hash -> Val
; se agrega un hash para incluir los campos de objetos disponibles
; en el scope (esto para simular el comportamiento de objetos visto
; en el OOPLAI
(define (interp expr env f-hash)
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r) (make-val (f (open-val (interp l env f-hash))
                                (open-val (interp r env f-hash))))]
    [(unop f s) (make-val (f (open-val (interp s env f-hash))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env f-hash))
     (if cnd
         (interp t env f-hash)
         (interp f env f-hash))]
    [(id x) (env-lookup x env)]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env f-hash)
                          (interp expr2 env f-hash))]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env f-hash))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (interp body new-env f-hash))]
    ; agregamos la interpretacion de las expr asociadas a objetos
    [(obj e-del-obj members)
     (def field-hash (hash-copy f-hash))
     (def method-hash (make-hash))
     (def del-obj (interp e-del-obj env f-hash))
     (def act-obj (objV field-hash method-hash del-obj))
     (interp-members members
                     (multi-extend-env (cons 'this '())
                                       (cons act-obj '())
                                       env))]
    [(this) (def act-obj (env-lookup 'this env)) ; obtenemos el obj actual del env
     (if (equal? act-obj 'outside-no-obj) ; si estamos fuera de un objeto, error
         (error "this used outside of an object")
         act-obj)] ;; si no entonces entregamos el objeto que se encuentra en this
    [(get fname) (def act-obj (env-lookup 'this env)) ; obtenemos el obj actual del env
     (if (equal? act-obj 'outside-no-obj) ; si estamos fuera de un objeto, error
         (error "get used outside of an object")
         (if (hash-has-key? f-hash fname) ; si estamos dentro de un obj, entonces buscamos en el f-hash
             (hash-ref f-hash fname)      ; si esta retornamos el valor
             (error "field not found")))] ; si no error
    [(set fname expr) (def act-obj (env-lookup 'this env)) ; obtenemos el obj actual del env
     (if (equal? act-obj 'outside-no-obj)  ; si estamos fuera de un objeto, error
         (error "set used outside of an object")
          (if (hash-has-key? f-hash fname) ; si estamos dentro de un obj, buscamos el campo
              (hash-set! f-hash fname (interp expr env f-hash)) ; si esta modificamos el valor
              (error "field not found")))] ; si no error
    [(send e-obj mname args)
     (def obj (interp e-obj env f-hash)) ; interpretamos la expr para obtener el objeto
     (interp-send obj obj mname args env f-hash)]
    [(shallow-copy expr)
     (def obj (interp expr env f-hash)) ; interpretamos el objeto a copiar
     (copy-obj obj (objV-parent obj))]
    [(deep-copy expr)
     (def obj (interp expr env f-hash)) ; interpretamos el objeto a copiar
     (interp-deep-copy obj)]
    [(null-obj) (voidV)] ; definimos el objeto nulo, el cual no entiende ningun mensaje
    ))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env Hash -> Expr
(define (interp-def a-def env f-hash)
  (match a-def
    [(my-def id body) (cons id (interp body env f-hash))]))

;; interp-memebers :: ListOf(Member) Env -> ObjV
;; funcion que recibe una lista de Member y muta los diccionarios del objeto asociado (this)
(define (interp-members members env)
  (def (objV field-hash method-hash parent) (env-lookup 'this env))
  (match members
    [(list (field-member fname fexpr) rest ...)
     (hash-set! field-hash fname (interp fexpr env field-hash))
     (interp-members rest env)]
    [(list (method-member mname margs mbody) rest ...)
     (hash-set! method-hash mname (mClosure margs mbody env))
     (interp-members rest env)]
    ['() (env-lookup 'this env)]))

;; interp-send :: ObjV ObjV Sym ListOf(Expr) Env Hash -> Val
;; funcion que interpreta el send a un objeto, recibe el objeto llamado y el current-object, tambien
;; recibe el nombre del metodo, los argumentos con que se llamara, el ambiente actual y el hash de campos
(define (interp-send obj current-obj mname args env f-hash)
  (if (hash-has-key? (objV-method-hash obj) mname) ; vemos si el objeto entiende el mensaje
         (match (hash-ref (objV-method-hash obj) mname) ; si lo entiende, entonces lo interpretamos
           [(mClosure margs body mEnv)
            (interp body
                    (multi-extend-env (cons 'this margs) ; extendemos el ambiente con el this del primer llamado
                                      (cons current-obj (map (λ (x) (interp x env f-hash)) args))
                                      mEnv)
                    (objV-field-hash obj))]) ; pero usamos los campos del objeto que sabe interpretar el mensaje
         (if (equal? (voidV) (objV-parent obj)) ; si no entiende el mensaje, vemos si podemos delegarlo
             (error "method not found") ; si no hay a quien delegar, error
             (interp-send (objV-parent obj) current-obj mname args env f-hash))))

;; copy-obj :: ObjV ObjV -> ObjV
;; genera un nuevo objeto, utilizando un objeto para obtener sus metodos y generar una copia de sus campos
;; tambien recibe un segundo campo que sera el parent del nuevo objeto
(define (copy-obj obj parent)
  (objV (hash-copy (objV-field-hash obj)) ; generamos una copia de los campos
        (objV-method-hash obj) ; reutilizamos los metodos
        parent)) ; usamos el parent que nos ingresaron

;; interp-deep-copy :: ObjV -> ObjV o 'undefined
(define (interp-deep-copy obj)
  (if (equal? (voidV) obj) ;; si el parent es nulo, entonces no seguimos generando copias
         obj ; si no esta definido el obj, entonces retornamos el nulo
         (copy-obj obj (interp-deep-copy (objV-parent obj)))))
         ; si no copiamos el objeto, entregandole una copia del padre

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) init-env (make-hash))) ;; agregamos el ambiente de inicio

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) init-env (make-hash))) ;; agregamos el ambiente de inicio
  (pretty-printing val))

;; pretty-printing Val -> SchemeVal
;; retorna valores de scheme asociados a los valores del lenguaje
(define (pretty-printing val)
  (match val
    [(voidV) "null"] ;; en caso del objeto nulo
    [(numV n) n]
    [(boolV b) b]
    [(objV field method parent)
     (list 'object (hash-keys field) (hash-keys method) (pretty-printing parent))]
    [x x]))