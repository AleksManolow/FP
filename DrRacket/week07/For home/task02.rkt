#lang racket
(require math/number-theory)

(define(how-del-num x)
  (define(helper curr)
    (cond
      [(zero? curr) 0]
      [(and (divides? curr x) (prime? curr)) (+ 1 (helper (- curr 1)))]
      [else (helper (- curr 1))]
      )
   )
    (helper (- x 1))
  )
(define(numbers n)
  (λ (k) (filter (λ (x) (< (how-del-num x) k) ) (range 1 (add1 n))))
   )
  

(equal? ((numbers 10) 1) '(1 2 3 5 7))
(equal? ((numbers 20) 2) '(1 2 3 4 5 7 8 9 11 13 16 17 19))