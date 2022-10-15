#lang racket
(define(growing-plant upSpeed downSpeed desiredHeight)
  (define (helper result up down desiredH)
    (cond
      [(>= upSpeed desiredHeight) 1]
      [(<=  desiredH 0) result]
      [(>= upSpeed desiredH) (+ result 1)]
      [else (helper (+ result 1)up down (- desiredH (- up down)))]
      )
    )
  (helper 0 upSpeed downSpeed desiredHeight)
  )






(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10); upSpeed=100, downSpeed=10, desiredHeight=910