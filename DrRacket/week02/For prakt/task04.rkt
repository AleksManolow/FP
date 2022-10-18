#lang racket

(require math/number-theory)

(define(num-prime? num)
  (define(helper curr-num)
    (cond
    [(>= curr-num num) #t]
    [(divides? curr-num num) #f]
    [(helper (+ curr-num 1))]
     )
    )
  (cond
    [(zero? num) (error "n was not negative")]
    [(= num 1) #f]
    [else(helper 2)]
     )
  )


(equal? (num-prime? 1) #f)
(equal? (num-prime? 2) #t)
(equal? (num-prime? 3) #t)
(equal? (num-prime? 6) #f)
(equal? (num-prime? 61) #t)