#lang racket

(require racket/trace)

(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
  )

(define(num-palindromes-rec a b)

  (cond
    [(> a b) 0]
    [(not (= a (rev a))) (num-palindromes-rec (+ a 1) b)]
    [else (+ 1 (num-palindromes-rec (+ a 1) b)) ]
    )
  )

  

(define(num-palindromes-iter a b)
  (define (helper result u v )
     (cond
    [(> u v) result]
    [(= u (rev u)) (helper (+ result 1) (+ u 1) v)]
    [else (helper result (+ u 1) v)]
    )
    )
 (helper 0 (min a b ) (max a b) )
  )


(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
;(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)