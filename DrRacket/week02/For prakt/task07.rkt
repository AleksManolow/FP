#lang racket

(define (inc-digits? n)
(cond
  [(< n 0) (error"n should be non-negative")]
  [(< n 10) #t]
  [ else (and
          (>= (remainder n 10) (remainder (quotient n 10) 10))
          (inc-digits? (quotient n 10)))]
  
  )
  )

(equal? (inc-digits? 1244) #t)
(equal? (inc-digits? 12443) #f)