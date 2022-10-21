#lang racket

(require racket/trace)

(define(last-digit num number-of-digits )
  (if(zero? num)
     number-of-digits
     (last-digit (quotient num 10) (+ number-of-digits 1) )
     )
  )


(define(automorphic? n)
  (cond
    [(<= n 0) (error "n was not natural")]
    [(= n (remainder (* n n) (expt 10 (last-digit n 0)))) #t]
    [else #f]
    )
    )

  





(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
; (automorphic? -1) ; error: n was not natural
; (automorphic? 0) ; error: n was not natural