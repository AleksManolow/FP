#lang racket

(require racket/trace)

(define(numerical-sequence  a b n)
    (if(= n 0)
       a
       (+ (* (expt 2 (- n 1)) b) (numerical-sequence a b (- n 1)))
       )
  )


(define (find-sum a b n)
  (+(numerical-sequence a b n) (numerical-sequence a b (- n 1)) (numerical-sequence a b (- n 2)) )
  )




(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98