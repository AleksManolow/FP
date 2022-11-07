#lang racket
(define(num-to-xs num)
  (define(helper result left-over)
    (if (zero? left-over)
        result
        (helper (cons (remainder left-over 10) result) (quotient left-over 10))
        )
    )
  (helper '() num)
  )

(define(xs-to-num xs)
  (define (helper result left-over)
    (if (null? left-over)
        result
        (helper (+ (* result 10 ) (car left-over)) (cdr left-over))
        )
    )
  (helper 0 xs)
  )






(equal? (num-to-xs 123) '(1 2 3))
(equal? (num-to-xs 123456789) '(1 2 3 4 5 6 7 8 9))

(= (xs-to-num '(1 2 3)) 123)
(= (xs-to-num '(1 2 3 4 5 6 7 8 9)) 123456789)