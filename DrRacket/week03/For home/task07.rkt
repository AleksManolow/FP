#lang racket

(define(p x)
  (/(- (* 3 (expt x 2)) x) 2)
  )

(define(p x)
  (/(- (* 3 (expt x 2)) x) 2)  )



(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)
(p 8)