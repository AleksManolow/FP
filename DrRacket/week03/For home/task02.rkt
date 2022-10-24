#lang racket

(require racket/trace)

(define(remove-first-occurrence num x)
  (define(helper result count n count-digits)
    (cond
      [(zero? n) result]
      [(and (= (remainder n 10) x) (zero? count)) (helper result (+ count 1) (quotient n 10) count-digits)]
      [else (helper (+ result (* (expt 10 count-digits)(remainder n 10))) count (quotient n 10) (+ count-digits 1))])
    )
  (helper 0 0 num 0)
  )

(define(find-max num max-el)
    (cond
      [(zero? num) max-el]
      [(> (remainder num 10) max-el) (find-max (quotient num 10) (remainder num 10))]
      [else (find-max (quotient num 10) max-el)]
      )
  )
(define (count-digits n)
  (if (< n 10)
      1
      (add1 (count-digits (quotient n 10)))
      )
  )

(define(sort-n num)
  (define(helper result num-rem-el count-digit)
    (if(and (zero? num-rem-el) (zero? count-digit))
       result
       (helper (+ (* result 10) (find-max num-rem-el 0)) (remove-first-occurrence num-rem-el (find-max num-rem-el 0)) (- count-digit 1))
     )
    )
  (helper 0 num (count-digits num))
  )

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)
