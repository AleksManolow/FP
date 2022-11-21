#lang racket

(define (deep-delete xss)
  (define (helper level left-over)
    (cond
      [(null? left-over) left-over]
      [(list? (car left-over)) (cons
                                (helper (add1 level) (car left-over))
                                (helper level (cdr left-over))
                                )]
      [(>= (car left-over) level) (cons
                                   (car left-over)
                                   (helper level (cdr left-over))
                                   )]
      [else (helper level (cdr left-over))]
     )
    )
  (helper 1 xss)
  )

(equal? (deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) '(1 (2 (4)) (3 ())))