#lang racket

(define(my-flatten xxs)
  (cond
    [(null? xxs) xxs]
    [(list? (car xxs)) (append (my-flatten (car xxs)) (my-flatten (cdr xxs)))]
    [else (cons (car xxs) (my-flatten (cdr xxs)))]
    )
  )

(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))
