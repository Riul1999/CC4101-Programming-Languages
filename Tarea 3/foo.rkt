#lang play
(require "main.rkt")
(print-only-errors)
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
