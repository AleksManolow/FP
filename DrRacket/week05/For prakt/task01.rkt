#lang racket
(define (sort-list xs)
  (λ (x) (sort xs x))

  )

(equal?
((sort-list '("one" "two" "0" "five" "" "one hundred" "onehundred")) (λ (x y) (< (string-length x) (string-length y))))
'("" "0" "one" "two" "five" "onehundred" "one hundred")
 )
