#lang racket
(define(rev-fold xs)
  (foldr (λ (x acc) (+ (* acc 10) x) ) 0 xs)
  )


(define(rev-lin-rec xs)
  (define(helper counter xs)
      (if(null? xs)
       0
       (+ (* (car xs) (expt 10 counter)) (helper (add1 counter) (cdr xs)))
     )
    )
  (helper 0 xs)
  )

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly recursive procedure
(= (rev-lin-rec '(1 2 3)) 321)
(= (rev-lin-rec '(1 2 3 4 5 6 7 8 9)) 987654321)