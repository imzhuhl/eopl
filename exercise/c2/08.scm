(define (empty-env) `())
(define (empty-env? env)
    (if (equal? env `())
        #t
        #f))