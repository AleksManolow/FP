#lang racket


(define(sum-digits-iter n)
  (define(helper sum-digit num )
    (cond
      [(< n 0)(error "n was negative")]
      [(zero? num) sum-digit]
      [else (helper (+ sum-digit (remainder num 10)) (quotient num 10))])
    )
  (helper 0 n)
  )




(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
; (sum-digits-iter -13) ; error "n was negative"