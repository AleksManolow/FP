#lang racket

(require math/number-theory)

(define (nth-cubic n)
  (define (helper counter num-cubic)
    (cond
      [(>= 0 n) (error "invalid value")]
      [(= counter n) (- (expt num-cubic 3) (expt (- num-cubic 1) 3))]
      [(prime? (- (expt(+ num-cubic 1) 3) (expt num-cubic 3))) (helper  (+ counter 1) (+ num-cubic 1))]
      [else (helper counter (+ num-cubic 1))])
    )
  (helper 0 1)
  )


(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61)
(= (nth-cubic 50) 55897)
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
;(nth-cubic 0) ; should return an error