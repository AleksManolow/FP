#lang racket
;task 01
(define(travel  result start xs);A function that calculates the smallest sequence of trips
  (cond
    [(null? xs) (reverse result)]
    [(pair? (assoc start xs)) (travel  (cons (cdr(assoc start xs)) result) (cdr(assoc start xs)) (remove (assoc start xs) xs) )]
    [else (error "No such itinerary!")]
    )
  )
(define(itinerary flights)
  (λ (start) (travel  (cons start '()) start flights)
    )
  )

(equal? ((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") 
("HKO" . "ORD"))) "YUL") '("YUL" "YYZ" "SFO" "HKO" "ORD") )

(equal? ((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) 
"A") '("A" "B" "C" "A" "C"))

;((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM") ;"No such itinerary!"

;task 02
(define(creat-first-and-last-row  result el len);A function that creates the first and last row of the matrix
  (if(zero? (+ len 2))
     result
     (creat-first-and-last-row (cons el result) el (sub1 len))
     ) 
  )

(define (crawl-matrix result el xs len);A function that encloses the matrix with the element X
  (cond
    [(null? xs) (reverse (cons (creat-first-and-last-row null el len) result))]
    [(null? result) (crawl-matrix (cons (creat-first-and-last-row null el len) result) el xs len)]
    [else (crawl-matrix (cons (reverse (cons el (reverse (cons el (car xs))))) result) el (cdr xs) len)])
  )

(define(pad xs)
  (λ (x) (crawl-matrix null x xs (length (car xs))))
  )

(equal? ((pad '( (1 2 3) (4 5 6) (7 8 9) ) ) 0) '( (0 0 0 0 0) (0 1 2 3 0) (0 4 5 6 0) (0 7 8 9 0) (0 0 0 0 0) ) ) 
(equal? ((pad '( (1 2 3) (4 5 6) (7 8 9) ) ) 9) '( (9 9 9 9 9) (9 1 2 3 9) (9 4 5 6 9) (9 7 8 9 9) (9 9 9 9 9) ) )