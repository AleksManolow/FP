#lang racket
(define(switchsum f g n)
  (λ (x) (if(= n 1)
            (f x)
            (+ (f x) ((switchsum g f (sub1 n)) (f x)))
            ))
  )


((switchsum (lambda (x) (+ x 1))  
            (lambda (x) (* x 2)) 1) 2) ;→ 3 
((switchsum (lambda (x) (+ x 1))  
            (lambda (x) (* x 2)) 2) 2) ;→ 9 
((switchsum (lambda (x) (+ x 1))  
            (lambda (x) (* x 2)) 3) 2) ;→ 16 
((switchsum (lambda (x) (+ x 1))  
            (lambda (x) (* x 2)) 4) 2) ;→ 30