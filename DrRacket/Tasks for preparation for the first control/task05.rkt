#lang racket
(define(sum-digit num)
  (if(zero? num)
     0
     (+ (remainder num 10) (sum-digit (quotient num 10)))
     )
  )

(define (sum-sum-digit a b k)
  (cond
    [(> a b) 0]
    [(zero? (remainder (sum-digit a) k)) (+ a (sum-sum-digit (add1 a) b k))]
    [else (sum-sum-digit (add1 a) b k)])
  )
(sum-sum-digit 10 13 2)