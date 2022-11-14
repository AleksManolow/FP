#lang racket
(define (assoc-rec key xs)
  (cond
    [(null? xs) (error "List was empty!")]
    [(= (length xs) 1) (if (= (caar xs) key) (cdar xs) (error "Element not present!"))]
    [(= (car (car xs)) key) (cdr (car xs))]
    [else (assoc-rec key (cdr xs))]
    )
  )


(define(assoc-hop el xs)
  (cdar (dropf xs (λ (x) (not (equal? el (car x))))))
  )

(define(assoc-assoc el xs)
  (cdr (assoc el xs))
  )

; using a recursive process
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using a higher order procedure
(equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using assoc
(equal? (assoc-assoc 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")