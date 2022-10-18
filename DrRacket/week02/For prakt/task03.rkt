#lang racket

(define(pow-rec x n)
  (cond
    [(= n 0) (error "error")]
    [(= n 1) x]
    [else (* x (pow-rec x (sub1 n))) ])
  )



(define(pow-iter x n)
  (define(helper result left-over)
    (if(= left-over 1)
     result
     (helper (* result x) (sub1 left-over) )
     )
    )
  (helper x n)
  )




(= (pow-rec 2 5) 32)
(= (pow-rec 15 3) 3375)

(= (pow-iter 2 5) 32)
(= (pow-iter 15 3) 3375)

;(pow-rec 2 0) ; should return an error (according to the task description)