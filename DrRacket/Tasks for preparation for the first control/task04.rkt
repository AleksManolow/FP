#lang racket

(define(repeater str)
  (λ (count glue)
    (define(helper result help-count)
      (if(zero? help-count)
         result
         (helper (string-append result glue str) (sub1 help-count))
         )
      )
    (helper str (sub1 count))
    )
  )

(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")