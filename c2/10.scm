(define (empty-env)
    '())

(define (extend-env var val env)
        (cons (cons var val) env))

(define (apply-env env search-var)
    (cond 
        ((null? env) (report-no-binding-found search-var))
        ((eqv? (caar env) search-var) (cdr (car env)))
        (else
            (apply-env (cdr env) search-var))))

(define (report-no-binding-found search-var)
    (error 'apply-env "No binding for: " search-var))

(define (has-binding? env s)
    (cond 
        ((null? env) #f)
        ((eqv? (caar env) s) #t)
        (else
            (has-binding? (cdr env) s))))

; 2.10
(define (extend-env* var-ls val-ls env)
    (if (null? var-ls)
        env
        (extend-env (car var-ls) (car val-ls) (extend-env* (cdr var-ls) (cdr val-ls) env))))



(define e
    (extend-env 'd 6
        (extend-env 'y 8
            (extend-env 'x 7
                (extend-env 'y 14
                    (empty-env))))))