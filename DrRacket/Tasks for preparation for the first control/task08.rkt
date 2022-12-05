#lang racket
(define (set-union xs ys)
  (sort (remove-duplicates (append xs ys)) <)
  )

(set-union '(1 3 5 7) '(5 7 13)) ;→ '(1 3 5 7 13) 
(set-union '(5 7 13) '(1 3 5 7)) ;→ '(1 3 5 7 13) 