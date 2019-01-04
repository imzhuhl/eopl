(define empty-env (lambda () '()))

(define (extend-env var val env)
        (cons (list (list var) (list val))
            env))

(define (extend-env* var-ls val-ls env)
    (if (null? var-ls)
        env
        (cons (list var-ls val-ls) env)))

(define (apply-env env search-var)
    (if (null? env) 
        (report-no-binding-found search-var)
        (let ((rst (apply-env-curr (caar env) (cadar env) search-var)))
            (if (car rst)
                (cdr rst)
                (apply-env (cdr env) search-var)))))

; etc. return (#t 'd)
(define (apply-env-curr vars vals search-var)
    (if (null? vars)
        (cons #f '())
        (if (eqv? (car vars) search-var)
            (cons #t (car vals))
            (apply-env-curr (cdr vars) (cdr vals) search-var))))            

(define (report-no-binding-found search-var)
    (error 'apply-env "No binding for: " search-var))

(define (has-binding? env s)
    (if (null? env)
        #f
        (let ((rst (apply-env-curr (caar env) (cadar env) search-var)))
            (if (car rst)
                #t
                (has-binding? (cdr env) s)))))   

(define e (empty-env))
; e '()

(define e (extend-env 'z 10 e))
; e '(((z) (10)))

(define e (extend-env* '(a b c d) '(1 2 3 4) e))
; e '( ((a b c d) (1 2 3 4)) ((z) (10)) )

(apply-env e 'z)