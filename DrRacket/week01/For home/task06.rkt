#lang racket
(define(rev x)
  (define(helper result num counter)
    (if(zero? num)
       result
       (helper (+ (* result 10)(remainder num 10)) (quotient num 10) (+ counter 1))
       )
    )
  (helper 0 x 0)
  )


(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)