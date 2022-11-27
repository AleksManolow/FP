#lang racket
(define(number-digit n)
  (if(zero? n)
     0
     (+ 1 (number-digit (quotient n 10)))
     )
  )
(define(sum-pow-dig n p)
  (if(zero? n)
     0
     (+ (expt (remainder n 10) p) (sum-pow-dig (quotient n 10) (sub1 p)))
     )
  )
(define(dig-pow n p)
  (if(zero? (remainder (sum-pow-dig n (+ (sub1 p) (number-digit n))) n))
     (quotient (sum-pow-dig n (+ (sub1 p) (number-digit n))) n)
     -1
     )
  )
(= (dig-pow 89 1) 1)
(= (dig-pow 92 1) -1)
(= (dig-pow 695 2) 2)
(= (dig-pow 46288 3) 51)


