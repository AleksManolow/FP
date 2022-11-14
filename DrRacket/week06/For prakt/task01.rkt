#lang racket
(define(my-reverse-iter xs)
  (define(helper result curr-xs)
    (if(null? curr-xs)
       result
       (helper (cons (car curr-xs) result) (cdr curr-xs))
       )
    )
  (helper null xs)
  )

(equal? (my-reverse-iter '(1 2 3 4 5)) '(5 4 3 2 1))