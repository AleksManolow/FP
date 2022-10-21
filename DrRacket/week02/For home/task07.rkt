#lang racket
(define (count-occurences number digit )
  (define(helper result n )
    (cond
      [(< number 0) (error "Negative number!")]
      [(zero? n) result]
      [(=(remainder n 10) digit) (helper (+ result 1)(quotient n 10))]
      [else (helper result (quotient n 10)) ]
      )
    )
  (helper 0 number)
  )



(= (count-occurences 121 1) 2)
(count-occurences -121 1) ; error "Negative number!"