#lang racket


(require math/number-theory)

(define (sum-divs n)
  (define (helper current-div sum)
    (cond
      [(>= current-div n) sum]
      [(divides? current-div n) (helper (add1 current-div) (+ sum current-div))]
      [else (helper (add1 current-div) sum)]
        )
  )
  (helper 1 0)
  )

(define (perfect? n)
  (if (not (positive? n))
      (error "n must be natural")
      (= n (sum-divs n))
      )
  )
(equal? (perfect? 6) #t)
(equal? (perfect? 33550336) #t)
(equal? (perfect? 495) #f)
(equal? (perfect? 1) #f)