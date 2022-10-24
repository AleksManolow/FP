#lang racket

(define (sum-digits-rec n)
  (cond
    [(< n 0) (error "n was not positive")]
    [(< n 10) n]
    [else (+ (remainder n 10) (sum-digits-rec (quotient n 10)))]
   )
  )

(define (sum-divisible-numbers start finish k)
  (define(helper counter-num )
    (cond
      [(> counter-num finish) 0]
      [(zero? (remainder (sum-digits-rec counter-num) k)) (+ counter-num (helper (add1 counter-num)))]
      [else (helper (add1 counter-num))]
      )
    )
    (if (> start finish)
      (sum-divisible-numbers finish start k)
      (helper start)
   )
  )

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)