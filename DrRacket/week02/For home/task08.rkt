#lang racket
(define(interesting? number)
       (define (helper sum num)
         (if(zero? num)
            sum
            (helper (+ sum (remainder num 10)) (quotient num 10))
            )
         )
       (if(zero? (remainder number (helper 0 number)))
          #t
          #f
          )
       )

(equal? (interesting? 410) #t)
