#lang racket
(define(triangular? mat)
  (define (helper count xs)
    (cond
      [(null? xs) #t]
      [(andmap (λ (x) (zero? x)) (take (car xs) count)) (helper (add1 count) (cdr xs))]
      [else #f]
      )
    )
  (helper 1 (cdr mat))
  )

(equal? (triangular? '( (1 2 3) (0 5 6) (0 0 9))) #t)
(equal? (triangular? '( (0 2 3) (0 0 6) (1 0 0))) #f)
(equal? (triangular? '( (1 2 3) (1 5 6) (0 0 9))) #f)
(equal? (triangular? '( (1 2 3 4) (0 5 6 7) (0 0 8 9) (0 0 0 10))) #t)

