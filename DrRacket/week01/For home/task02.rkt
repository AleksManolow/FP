#lang racket
(define(leap-year-one-line? x)
 *(and (zero? (remainder x 4))
       (or (not(zero? (remainder x 100)))
        (zero? (remainder x 400)) ))
  )

(define(is-leap-year-guards? x)
  (cond
   [(and(zero? (remainder x 4)) (zero? (remainder x 100))(zero? (remainder x 400)))#t]
   [(and(zero? (remainder x 4)) (zero? (remainder x 100)))#f]
   [(zero? (remainder x 4))#t]
   [else #f]
  )
  )

(equal? (leap-year-one-line? 2020) #t)
(equal? (leap-year-one-line? 1988) #t)
(equal? (leap-year-one-line? 1600) #t)
(equal? (leap-year-one-line? 2400) #t)
(equal? (leap-year-one-line? 2023) #f)
(equal? (leap-year-one-line? 1700) #f)
(equal? (leap-year-one-line? 1800) #f)
(equal? (leap-year-one-line? 2100) #f)

(equal? (is-leap-year-guards? 2020) #t)
(equal? (is-leap-year-guards? 1988) #t)
(equal? (is-leap-year-guards? 1600) #t)
(equal? (is-leap-year-guards? 2400) #t)
(equal? (is-leap-year-guards? 2023) #f)
(equal? (is-leap-year-guards? 1700) #f)
(equal? (is-leap-year-guards? 1800) #f)
(equal? (is-leap-year-guards? 2100) #f)
