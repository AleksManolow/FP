#lang racket
(define(count-digits-iter n)
  (define (helper count-digits num)
    (cond
      [(< num 0)(error "n was negative")]
      [(<= num 1) (+ count-digits 1)]
      [else (helper (+ count-digits 1)(quotient num 10))])
    )
  (helper 0 n)
  )




(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
(count-digits-iter -13) ; error "n was negative"