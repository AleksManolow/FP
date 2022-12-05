#lang racket

;task 01 
(define(sum-digit x);A function that gives the sum of the digits
      (if (< x 10)
       x
      (+ (remainder x 10) (sum-digit (quotient x 10)))
   )
  )

(define(meet-number  digit number);A function that gives how many times a digit occurs in a number
   (cond
     [(zero? number) 0]
     [(= digit (remainder number 10)) (+ 1 (meet-number  digit (quotient number 10)))]
     [else  (meet-number  digit (quotient number 10))])
)


(define(sum-counts-iter x d)
 (define (helper result counter start-of-the-interval)
   (cond
     [(or(< d 0))(<= x 0) (error "invalid value")]
     [(> start-of-the-interval x) result]
     [(> (meet-number d start-of-the-interval) 0)
      (helper (sum-digit (+ counter (meet-number  d start-of-the-interval)))
              (+ counter (meet-number  d start-of-the-interval))
              (+ start-of-the-interval 1))]
     [else (helper result counter (+ start-of-the-interval 1))]
     )
   )
  (helper 0 0 1)
  )

(sum-counts-iter 1 1)      ; -> 1 
(sum-counts-iter 5123 1)   ; -> 19 
(sum-counts-iter 1234 8)   ; -> 10 
(sum-counts-iter 5555 5)   ; -> 10 
(sum-counts-iter 65432 6)  ; -> 11 
(sum-counts-iter 70000 1)  ; -> 11 
(sum-counts-iter 123321 1) ; -> 29


;task 02

(define (add-ones  n)
  (define (helper result x counter)
    (cond
      [(< n 0) (error "invalid value")]
      [(zero? x) result]
      [(= (+ (remainder x 10) 1) 10) (helper (+ result  (* 10 (expt 10 counter))) (quotient x 10) (+ counter 2))]
      [else (helper (+ result (* (+ (remainder x 10) 1) (expt 10 counter))) (quotient x 10) (+ counter 1))]
      )
    )
  (if(zero? n)
     1
     (helper 0 n 0)
     )
  )
(add-ones 123)  ; -> 234 
(add-ones 193)  ; -> 2104 
(add-ones 998)  ; -> 10109 
(add-ones 9999) ; -> 10101010 
   
