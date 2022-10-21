#lang racket
(require racket/trace)

(define(sum-of-del n)
  (define(helper sum counter)
    (cond
      [(>= counter n) sum]
      [(zero? (remainder n counter)) (helper (+ sum counter) (+ counter 1))]
      [else (helper sum (+ counter 1))])
    )
  (helper 0 1)
  )

(define(amicable? x y)
  (if(= (sum-of-del x) y)
     #t
     #f
     )
  )
(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)