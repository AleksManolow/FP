#lang racket
(define(count-occurrences ys xs)
  (cond
    [(> (length ys) (length xs)) 0]
    [(equal? ys (take xs (length ys))) (add1 (count-occurrences ys (cdr xs)))]
    [else (count-occurrences ys (cdr xs))]
    )
  )




(= (count-occurrences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurrences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurrences '(6 6) '(2 2)) 0)