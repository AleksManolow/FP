#lang racket

(define(insert-at x idx xs)
    (if(zero? idx)
       (cons x xs)
       (cons (car xs) (insert-at x (sub1 idx) (cdr xs)))
       )
    )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))