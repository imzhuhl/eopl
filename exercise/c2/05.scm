(define (empty-env) `())

(define (extend-env var val env)
    (cons (cons var val) env))

(define (apply-env env search-var)
    (cond 
        ((null? env) (report-no-binding-found search-var))
        ((eqv? (caar env) search-var) (cdr (car env)))
        (else
            (apply-env (cdr env) search-var))))

(define (report-no-binding-found search-var)
    (eopl:error `apply-env "No binding for ~s" search-var))


(define e
    (extend-env `d 6
        (extend-env `y 8
            (extend-env `x 7
                (extend-env `y 14
                    (empty-env))))))