#lang racket
(define (snail heightColumn distanceCrawlsDuringTheDay distanceSlidesDownDuringTheNight)
  (define(helper result heigh up down)
    (cond
     [(>= up heightColumn) 1]
     [(= heigh 0) result]
     [(>= up heigh) (+ 1 result)]
     [else (helper (+ 1 result) (- heigh (- up down)) up down)]
     )
    )
  (helper 0 heightColumn distanceCrawlsDuringTheDay distanceSlidesDownDuringTheNight)
  )







(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)