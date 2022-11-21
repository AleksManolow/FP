#lang racket
(define (graph-contains-points xs f)
  (andmap (λ (x) (= (cdr x) (f (car x)))) xs)
  )



(equal? (graph-contains-points '((1 . 2) (2 . 3) (3 . 4)) (λ (x) (+ x 1))) #t)
(equal? (graph-contains-points '((1 . 2) (2 . 4) (3 . 4)) (λ (x) (+ x 1))) #f)