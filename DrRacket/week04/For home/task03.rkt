#lang racket
(define(apply-n f n)
  (λ (x)
    (if(= n 1)
       (f x)
       ( (apply-n f (sub1 n)) (f x))
       ) 
    )
  )



(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)