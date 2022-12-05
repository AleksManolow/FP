#lang racket

(define(where list-elements list-predicates)
  (if(null? list-predicates)
     list-elements
     (where (filter (car list-predicates) list-elements)  (cdr list-predicates))
     )
  )

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10))
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '())





