(define (has-binding? env s)
    (cond 
        ((null? env) #f)
        ((eqv? (caar env) s) #t)
        (else
            (has-binding? (cdr env) s))))