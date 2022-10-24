#lang racket

(define(remove-first-occurrence num x)
  (define(helper result count n count-digits)
    (cond
      [(zero? n) result]
      [(and (= (remainder n 10) x) (zero? count)) (helper result (+ count 1) (quotient n 10) count-digits)]
      [else (helper (+ result (* (expt 10 count-digits)(remainder n 10))) count (quotient n 10) (+ count-digits 1))])
    )
  (helper 0 0 num 0)
  )


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)