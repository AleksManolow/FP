#lang racket

(require math/number-theory)

(define(sum-divs num)
  (define(helper sum-of-del curr-num)
    (cond
    [(= curr-num 1) (+ sum-of-del 1)]
    [(divides? curr-num num) (helper (+ sum-of-del curr-num) (sub1 curr-num))]
    [else (helper sum-of-del (sub1 curr-num))]
     )
    )
  
  (if(< num 1)
     0
     (helper 0 num)
     )
  )



(= (sum-divs 0) 0)
(= (sum-divs 1) 1)
(= (sum-divs 6) 12) ; 1 + 2 + 3 + 6
(= (sum-divs 12345) 19776)