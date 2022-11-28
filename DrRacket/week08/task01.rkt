#lang racket
(define(have-zero xs counter)
  (if(zero? (car xs))
     counter
     (have-zero (cdr xs) (add1 counter))
     )
  )

(define (trnasorm-row-in-matrix xs count num-of-el)
  (cond
    [(null? xs) xs]
    [(= count num-of-el) (cons 0 (trnasorm-row-in-matrix (cdr xs) (add1 count) num-of-el))]
    [else (cons (car xs) (trnasorm-row-in-matrix (cdr xs) (add1 count) num-of-el))]
    )
  )

(define(trnsform-matrix xs count)
  (if(null? xs)
     xs
     (cons (trnasorm-row-in-matrix (car xs) 0 count) (trnsform-matrix (cdr xs) count))
     )
  )

(define(zero-cols xxs)
  (define(helper matrix new-matrix)
    (cond
      [(null? matrix) new-matrix]
      [(list?(member 0 (car matrix))) (helper (cdr matrix) (trnsform-matrix new-matrix (have-zero (car matrix) 0)))]
      [else (helper (cdr matrix) new-matrix)]
      )
    )
  (helper xxs xxs)
  )

(equal? (zero-cols '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 2 0) (0 4 0) (0 5 0) (0 2 0)))
