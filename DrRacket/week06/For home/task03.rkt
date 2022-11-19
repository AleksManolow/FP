#lang racket
(define(bigger-el el xs)
  (cond
    [(null? xs) 0]
    [( < el (car xs)) (add1 (bigger-el el (cdr xs)))]
    [else (bigger-el el (cdr xs))])
  )

(define(num-bigger-elements xs)
  (define (helper result left-over)
      (if(null? left-over)
         (reverse result)
         (helper (cons (cons (car left-over) (bigger-el (car left-over) xs)) result) (cdr left-over))
     )
    )
  (helper null xs)
  )

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
