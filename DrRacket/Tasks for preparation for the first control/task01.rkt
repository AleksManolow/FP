#lang racket
(define(descending-order n)
  (cond
    [(zero? (quotient n 10)) #t]
    [(<= (remainder n 10) (remainder (quotient n 10) 10)) (descending-order (quotient n 10))]
    [else #f])
  )

(define (sum-numbers a b)
  (cond
    [(> a b) 0]
    [(descending-order a)  (+ a (sum-numbers (add1 a) b))]
    [else (sum-numbers (add1 a) b)]
    )
  )
(sum-numbers 1 9); → 45 
(sum-numbers 199 203); → 200 
(sum-numbers 219 225); → 663
