#lang eopl

(define value?
  (lambda (v)
    #t))

(define-datatype env env?
  (empty-env-inter)
  (apply-env-inter
   (_var symbol?)
   (_env env?))
  (extend-env-inter
   (_var symbol?)
   (_val value?)
   (_env env?))
  (has-binding-inter
   (_var symbol?)
   (_env env?)))

(define empty-env
  (lambda ()
    (empty-env-inter)))

(define extend-env
  (lambda (var val E)
    (extend-env-inter var val E)))

(define apply-env
  (lambda (var E)
    (cases env E
      (empty-env-inter ()
                       (eopl:error 'apply-env "Empty env"))
      (extend-env-inter (_var _val _env)
                        (if (eqv? _var var)
                            _val
                            (apply-env var _env)))
      (apply-env-inter (_var _env)
                       (eopl:error 'apply-env "error"))
      (has-binding-inter (_var _env)
                         (eopl:error 'apply-env "error")))))

(define has-binding?
  (lambda (var E)
    (cases env E
      (empty-env-inter () #f)
      (extend-env-inter (_var _val _env)
                        (if (eqv? _var var)
                            #t
                            (has-binding? var _env)))
      (apply-env-inter (_var _env)
                       (has-binding? var _env))
      (has-binding-inter (_var _env)
                         (has-binding? var _env)))))


(define e (extend-env 'b 3 (extend-env 'a 2 (extend-env 'a 1 (empty-env)))))
