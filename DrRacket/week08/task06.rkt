#lang racket
(define(diagonal xss)
  (define(helper xs count result)
    (if(null? xs)
       (reverse result)
       (helper (cdr xs) (add1 count) (cons (list-ref (car xs) count) result))
       )    
    )
  (helper xss 0 '())
  )

(equal? (diagonal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) '(1 6 11 16))