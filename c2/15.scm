(define (var-exp var)
    (cons 'var-exp var))

(define (lambda-exp var lc-exp)
    (list 'lambda-exp (list var) lc-exp))

(define (app-exp lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2))

(define (var-exp? x)
    (and (pair? x) (eqv? (car x) 'var-exp)))

(define (lambda-exp? x)
    (and (list? x) (eqv? (car x) 'lambda-exp)))

(define (app-exp? x)
    (and (list? x) (eqv? (car x) 'app-exp)))

(define (var-exp->var x)
    (cdr x))

(define (lambda-exp->bound-var x)
    (caadr x))

(define (lambda-exp->body x)
    (caddr x))

(define (app-exp->rator x)
    (cadr x))

(define (app-exp->rand x)
    (caddr x))

(define (occurs-free? search-var exp)
    (cond
        ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
        ((lambda-exp? exp)
            (and
                (not (eqv? search-var (lambda-exp->bound-var exp)))
                (occurs-free? search-var (lambda-exp->body exp))))
        (else
            (or
                (occurs-free? search-var (app-exp->rator exp))
                (occurs-free? search-var (app-exp->rand exp))))))


(occurs-free? 'a (var-exp 'a))
(occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))


