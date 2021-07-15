#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    ; Listas 2
    [(list 'list vals ...) (parse (parse-list vals))]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]
    ))

; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    ; Listas 2
    [(list 'list args ...) (parse-pattern (parse-list args))]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x) (env-lookup x env)]
    ; function (notice the meta interpretation)
    [(fun ids body) ; Aqui agregamos do-lazy para generar promesas
     (λ (arg-vals args-env)
       (interp body (extend-env (delete-lazy ids) (do-lazy ids arg-vals args-env) env)))]
    ; application
    [(app fun-expr arg-expr-list) ; Eliminamos la evaluacion temprana
     ((strict (interp fun-expr env)) arg-expr-list env)] ; Asi podemos tener lazyness
      ;(map (λ (a) (interp a env)) arg-expr-list))]
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (strict (interp a env))) arg-expr-list))]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (strict (interp expr env)))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (strict (interp body (extend-env (map car alist) (map cdr alist) env)))]))
    ; Se agrega punto de strictness, para el caso en que se retorna solo un id

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v env) (symbol=? (structV-name (interp (first v) env)) name))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname ; agregamos variable extra para interpretar los args
               (λ (args env)
                 (structV name
                          varname ; si el id asoc al arg es lazy, dejamos un promesa
                          (do-lazy (variant-params var) args env)
                          ;(map (λ (a) (interp a env)) args)
                          ))
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               ; Agregamos env para poder interpretar el arguento
               (λ (v env) (symbol=? (structV-variant (interp (first v) env)) varname))
               env))

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern (strict value)))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [(x y) (match/values (values pattern (strict value))
                                     [((constrP ctr patterns) (structV _ ctr-name str-values))
                                      (if (symbol=? ctr ctr-name)
                                          (apply append (map match-pattern-with-value
                                                             patterns str-values))
                                          (list #f))]
                                     [(x y) (error "Match failure")])]))

;; run :: s-expr -> number/boolean/procedura/struct/string
(define(run prog [flag ""])
  (def real-prog `{local {,list-type ,length-fun}
                     ,prog})
  (define res (strict (interp (parse real-prog) empty-env)))
  (if (Struct? res)
      (if (equal? flag "ppwu")
          (pretty-printing res)
          (if (equal? flag "pp")
              (pretty-printing-list res)
              (extract-env res)))
      (extract-env res)))


#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (if (equal? (second args) 0)
                               (error "division by zero")
                               (apply / args))))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))

;--------------------------------------------------------------------
;--------------------------------------------------------------------
; Aqui empieza codigo escrito por Rodrigo Urrea
; Tambien hay codigo incrustado en las funciones definidas anteriormente,
; pero la mayor cantidad de codigo nuevo se encuentra aqui.

; pretty-printing :: Struct -> String
; Funcion que toma un Struc de MiniScheme+ y retorna una String
; de Scheme que representa de forma mas legible a la Struct
(define (pretty-printing struc)
  (match struc
    [(structV name var vals)
     (string-append "{" (symbol->string var) (printing-pretty vals) "}")]))

; printing-pretty :: ListOf(number/boolean/procedura/struct/string) -> String
; Funcion auxiliar de pretty-printing que toma un lista de elementos y retorna
; un String de Scheme que representa de forma mas legible a la Struct.
(define (printing-pretty values)
  (match values
    [(list (? structV? struct ) rest ...)
     (string-append " " (pretty-printing struct) (printing-pretty rest))]
    [(list (? exprV? prom) rest ...) (string-append (printing-pretty (list (strict prom))) (printing-pretty rest))]
    [(list val rest ...) (string-append " " (format "~a" (strict val)) (printing-pretty rest))]
    [else  ""]))

;Tipo <List> ::=
; | <Empty>
; | (Cons <Expr> <List>)
(define list-type '{datatype List
                             {Empty}
                             {Cons expr lst}})

; length :: List -> num
; Funcion que calcula el largo de una lista
(define length-fun '{define length {fun {lst}
                                        {match lst
                                          {case {Empty} => 0}
                                          {case {Cons expr lst} => {+ 1 {length lst}}}}}})
; parse-list :: ListOf(s-expr) -> s-expr
; Funcion que recibe los elementos dentro del azucar sintactico ({list e1 e2 ... en})
; y los tranforma en  sintaxis concreta del tipo List ({Cons e1 {Cons e2 ... {Cons en {Empty}}}})
(define (parse-list args)
  (match args
    [(list head tail ...) `{Cons ,head ,(parse-list tail)}]
    ['() `{Empty}]))

; pretty-printing-list :: Struct -> String
; Recorre la estructura generando el mismo comportamiento que pretty-printing,
; pero si se encuentra con una Struct de nombre 'List, el string que retorna
; es {list e1 e2 ... en}
(define (pretty-printing-list struct)
  (if (equal? 'List (structV-name struct))
      (string-append "{list"
                     (pretty-list struct)
                     "}")
      (string-append "{"
                     (symbol->string (structV-variant struct))
                     (printing-pretty-list (structV-values struct))
                     "}")
      ))
; printing-pretty-list :: ListOf(Any) -> String
; Transforma todos los elementos de una lista en su forma de string,
; cuando encuentra una Struct llama a pretty-printing-list
(define (printing-pretty-list values)
  (match values
    [(list (? structV? struct ) rest ...)
     (string-append " "
                    (pretty-printing-list struct)
                    (printing-pretty-list rest))]
    [(list (? exprV? prom) rest ...)
     (string-append (printing-pretty-list (list (strict prom)))
                    (printing-pretty-list rest))]
    [(list val rest ...) (string-append " "
                                        (format "~a" (strict val))
                                        (printing-pretty-list rest))]
    [else  ""]))

; pretty-list :: structV(List) -> String
; recorre la struct List y entrega un string mas amigable de la lista
(define (pretty-list struct)
  (if (equal? 'Cons (structV-variant struct))
      (string-append (printing-pretty-list (list (car (structV-values struct))))
                     (pretty-list (second (structV-values struct))))
      ""))

; Promesa :: {exprV arg env cache}
; Estructura para almacenar una promesa de evaluacion, y asi tener
; evaluacion perezosa. Estas son consideradas valores del lenguaje,
; pero nunca se retornaran como tales
(deftype Promise
  [exprV arg env cache]
  [exprV-WE arg]) ;esta promesa es solo para mostrarla como resultado

; delete-lazy :: ListOf(sym|(lazy sym)) -> ListOf(sym)
; Transforma todos los (lazt sym) en sym, para que funcione correctamente
; el env-lookup
(define (delete-lazy ids)
  (match ids
    [(list (list 'lazy id) rest ...) (cons id (delete-lazy rest))]
    [(list id rest ...) (cons id (delete-lazy rest))]
    ['() '()]))

; do-lazy :: ListOf(sym|(lazy sym)) ListOf(Expr) Env -> ListOf(Val)
; Funcion retorna el correspondiente valor de la expresion, en caso
; de que el id asociado sea de la forma (lazy sym),  se guarda una
; promesa del valor
(define (do-lazy ids args env)
  (match ids
    ['() (match args
           ['() '()]
           [else (error 'interp "Muchos argumentos aplicando funcion")])]
    [(list id rest-ids ...) (match args
                             ['() (error 'interp "Pocos argumentos aplicando funcion")]
                             [(list arg rest-args ...)
                              (match id
                                [(list 'lazy sym) (cons (exprV arg env (box 'undefined)) (do-lazy rest-ids rest-args env))]
                                [else (cons (strict (interp arg env)) (do-lazy rest-ids rest-args env))])])]))

; strict : number/boolean/procedura/struct/string/Promise -> number/boolean/procedura/struct/string
(define (strict val)
  (match val
    [(exprV expr env cache)
     (if (not (equal? 'undefined (unbox cache)))
         (begin
           (unbox cache))
         (let ([inval (strict (interp expr env))])
           (set-box! cache inval)
           inval))]
    [_ val]))

; extract-env :: number/boolean/procedura/struct/string/Promise -> number/boolean/procedura/struct/string/Promise
; Examina el valor ingresado y en caso de encontrar una Promise, se
; le quita el ambiente
(define (extract-env val)
  (match val
    [(? exprV?) (exprV-WE (exprV-arg val))]
    [(? structV?) (structV (structV-name val)
                           (structV-variant val)
                           (map extract-env (structV-values val)))]
    [else val]))

; stream-data
; tipo de dato que representa las listas en que la cola es lazy
(def stream-data '{datatype Stream
                             {stream expr {lazy lst}}})

; make-stream :: Expr Expr -> Stream
; funcion que permite crear un stream infinito, basado en la estructura Stream
(def make-stream '{define make-stream {fun {hd {lazy tl}}
                                        {stream hd tl}}})

; ones
; lista infinita de unos
(def ones '{define ones {make-stream 1 ones}})

; stream-hd :: Stream -> val
; funcion que dado un stream retorna su primer elemento
(def stream-hd '{define stream-hd
                  {fun {str}
                       {match str
                         {case {stream hd tl} => hd}}}})

; stream-tl :: Stream -> Stream
; funcion que dado un stream retorna su segundo elemento (stream)
(def stream-tl '{define stream-tl
                  {fun {str}
                       {match str
                         {case {stream hd tl} => tl}}}})

; stream-take :: num Stream -> ListOf<val>
; funcion que retorna los n primeros valores del stream
(def stream-take '{define stream-take
                    {fun {n str}
                         {if {= n 0}
                             {Empty}
                             {Cons {stream-hd str}
                                   {stream-take {- n 1} {stream-tl str}}}}}})
; stream-lib
; lista que contiene todas las funciones relacionadas con streams
(def stream-lib (list stream-data
                      make-stream
                      stream-hd
                      stream-tl
                      stream-take))

; stream-zipWith :: (val val -> val) Stream Stream -> Stream
; funcion que opera "todos" los elementos de 2 streams
(def stream-zipWith '{define stream-zipWith
                      {fun {op str1 str2}
                           {stream
                            {op {stream-hd str1} {stream-hd str2}}
                            {stream-zipWith op
                                            {stream-tl str1}
                                            {stream-tl str2}}}}})

; fibs
; Stream que representa la sucesion de fibonacci
(def fibs '{define fibs
             {make-stream 1
                          {make-stream 1
                                       {stream-zipWith
                                        {fun {x y} {+ x y}}
                                        fibs
                                        {stream-tl fibs}}}}})

; merge-sort :: Stream Stream -> Stream
; funcion que dado 2 streams ordenados retorna un stream
; ordenada con elementos de ambos streams
(def merge-sort '{define merge-sort
                   {fun {str1 str2}
                        {if {< {stream-hd str1} {stream-hd str2}}
                            {stream {stream-hd str1}
                                    {merge-sort {stream-tl str1}
                                                str2}}
                            {stream {stream-hd str2}
                                    {merge-sort str1
                                                {stream-tl str2}}}}}})