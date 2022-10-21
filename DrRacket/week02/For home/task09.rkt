#lang racket

(define(max-multiple d b )
  (define(helper num count-num)
      (cond
    [(> count-num b) num]
    [(zero? (remainder count-num d)) (helper count-num  (+ count-num 1))]
    [else (helper num  (+ count-num 1)) ]
    ))

  (helper 0 1)
  )






(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)