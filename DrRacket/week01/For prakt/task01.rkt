#lang racket
(define (my-min-built-in-p x y)
  (min x y)
  )


(define(my-min-if x y)
  (if(< x y)
  x
  y )
  )

(define (my-min-guard x y)
  (cond
    [(< x y) x]
    [else y]
    )
  )

(define(last-digit x)
  (remainder x 10)
  )

(define(quotient-whole x y)
  (quotient x y))

(define (div-whole x y)
  (/ x y))

(define (remove-last-digit x)
  (quotient x 10))

(define (div-real x y)
  (/ x y)
  )

(define(round-two-dig x)
  (/(round(* x 100))100)
  )

(define(average-whole x y)
 (/(+ x y) 2)
  )

(= (my-min-built-in-p 5 6) 5)

(= (my-min-if -60 -15) -60)
(= (my-min-if 15 60) 15)
(= (my-min-if 60 15) 15)

(= (my-min-guard 15 60) 15)
(= (my-min-guard 60 15) 15)

(= (last-digit 154) 4)

(= (quotient-whole 64 2) 32)

(div-whole 154 17) ; 9 1/17

(= (remove-last-digit 154) 15)

(= (div-real 154 10) 15.4)

(= (round-two-dig pi) 3.14)


(= (average-whole 5 1542) 773.5)