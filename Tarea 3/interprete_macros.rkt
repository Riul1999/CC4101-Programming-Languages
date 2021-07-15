#lang play
(defmac (OBJECT-DEL parent
                    ([field fname fval] ...)
                    ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method
  #:captures self !
  (let* ([fname fval] ...)
    (let ([methods
           (list (cons 'mname (λ (self mparam ...) mbody ...)) ...)])
      (λ (current)
        (λ (msg . args)
          (let ([found (assoc msg methods)])
            (if found
                (apply (cdr found) (cons current args))
                (apply (parent current) msg args))))))))

;(defmac (! f v) #:captures self
;              (dict-set! (obj-values self) 'f v))

(define o (OBJECT-DEL #f
                      ([field x 10] [field y 20])
                      ([method createb () (OBJECT-DEL #f
                                                      ()
                                                      ([method get-x () x]))])))

(define p (OBJECT-DEL #f
                      ([field x 10] [field y 20])
                      ([method createb () (OBJECT-DEL #f
                                                      ([field y 99] [field x y])
                                                      ([method get-x () x]))])))

(define q (OBJECT-DEL #f
                      ([field x 10])
                      ([method get-y () 10])))

(define r (OBJECT-DEL q
                      ([field y 10])
                      ()))

(define s (OBJECT-DEL #f
                      ([field x 10])
                      ([method get-x () x]
                       [method createb () (OBJECT-DEL #f
                                                      ()
                                                      ([method set-x (val) (set! x 20)]
                                                       [method get-x () x]))])))

(defmac (→ o m arg ...)
  (let ([obj o])
    ((obj obj) 'm arg ...)))

;(→ (→ p createb) get-x)
;(→ (→ s createb) set-x 10)
;(→ (→ s createb) get-x)
;(→ s get-x)
(→ (→ o createb) get-x)
