#lang racket
(define(concat-proc xs ys)
  (append xs ys)
  )

(define(concat-rec xs ys)
  (if(null? ys)
     (flatten xs)
     (concat-rec (cons xs (car ys)) (cdr ys))
     ) 
  )

; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly recursive process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))