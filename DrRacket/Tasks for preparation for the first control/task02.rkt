#lang racket
(define(big-el el xs)
  (cond
    [(null? xs) 0]
    [( < el (car xs)) (+ 1 (big-el el (cdr xs)))]
    [else (big-el el (cdr xs))])
  )
(define (num-bigger-elements lst)
  (define (helper xs)
     (if(null? xs)
     '()
     (cons (cons (car xs) (big-el (car xs) lst)) (helper (cdr xs)))
    )
  )
  (helper lst)
)

(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))
