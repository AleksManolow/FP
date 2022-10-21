#lang racket

(require math/number-theory)

(define (numbers-contain-digit n d)
  (cond
    [ (zero? n) #f]
    [(= (remainder n 10) d) #t]
    [else (numbers-contain-digit (quotient n 10)  d) ])
  )


(define(sum-special-primes n d)
  (define (helper sum counter num)
    (cond
      [(= counter n) sum]
      [(and (prime? num) (numbers-contain-digit num d)) (helper (+ sum num) (+ counter 1) (+ num 1))]
      [else (helper sum counter (+ num 1))]
      )

    )
  (helper 0 0 1)
  )



(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)