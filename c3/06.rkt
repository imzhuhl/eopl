#lang eopl

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

#|
(define-datatype program program
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (id symbol?))
  (zero?-exp
   (expr expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (let-exp
   (var symbol?)
   (value expression?)
   (body expression?))
  (minus-exp
   (body-exp expression?)))
|#

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?)))

;; env
(define-datatype env environment?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val (lambda (x) #t))
   (saved-env environment?)))

(define init-env
  (lambda ()
    (empty-env)))

(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

(define (expval->bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool v))))
    
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1) (value-of exp1 (init-env)))))
    ;(a-program (exp1) (display exp1))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
              (let ([val1 (value-of exp1 env)]
                    [val2 (value-of exp2 env)])
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num val2)])
                  (num-val (- num1 num2)))))
    (zero?-exp (exp1)
               (let ([val1 (value-of exp1 env)])
                 (let ([num (expval->num val1)])
                   (if (zero? num)
                       (bool-val #t)
                       (bool-val #f)))))
    (if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of exp1 env)])
              (if (expval->bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    (let-exp (var exp1 body)
             (let ([val1 (value-of exp1 env)])
               (value-of body (extend-env var val1 env))))
    (minus-exp (body-exp)
               (let ([val1 (value-of body-exp env)])
                 (let ([num (expval->num val1)])
                   (num-val (- 0 num)))))))
    

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors ": ~s, ~s" variant value))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))


(define run
  (lambda (string)
    (value-of-program (scan&parse string))))




    

