#lang racket

(require math/number-theory)


(define(sum-prime-divs-rec n)
  (define (helper counter)
      (cond
    [(zero? counter) 0]
    [(and (zero? (remainder n counter))(prime? counter)) (+ counter (helper (- counter 1)))]
    [else  (helper (- counter 1))])
    )
  (helper n)
  )


(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)