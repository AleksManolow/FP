#lang racket
(require math/number-theory)
(define(sum-digit-divisors x)
  (define(helper curr)
    (cond
      [(zero? curr) 0]
      [(divides? (remainder curr 10) x) (+ (remainder curr 10) (helper (quotient curr 10)) ) ]
      [else (helper (quotient curr 10))])
    )
   (if (>= x 1)
      (helper x)
      (error "x was not natural")
      )
  )






(= (sum-digit-divisors 1) 1)
(= (sum-digit-divisors 28) 2)
(= (sum-digit-divisors 32) 2)
(= (sum-digit-divisors 29) 0)
(= (sum-digit-divisors 34) 0)
(= (sum-digit-divisors 1048) 13)