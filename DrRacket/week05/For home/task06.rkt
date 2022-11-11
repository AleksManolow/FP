#lang racket
(require math/number-theory)

(define(factorize num)
   (define(helper result left-over)
     (cond
       [(< left-over 2) (flatten result)]
       [(and (zero? (remainder num left-over)) (prime? left-over)) (helper (cons left-over result) (sub1 left-over))]
       [else (helper result (sub1 left-over))]
       )
     )
  (helper '() num)
   )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))
