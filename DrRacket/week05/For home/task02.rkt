#lang racket
(define(new-function trp os uo)
  (cond
    [(and (zero? (length os)) (<= (length uo) (length trp))) ()]


    )
  )
(define(longest-ascending-sub xs)
  (if(< (length xs) 1)
     xs
     
     )
  )





(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))