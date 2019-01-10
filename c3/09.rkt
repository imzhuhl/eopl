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
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)

    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("null?" "(" expression ")") null?-exp)

    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
#|
(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (const-exp
   (num number?))
  (zero?-exp
   (expr expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (predicate-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (minus-exp
   (body-exp expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (let-exp
   (var symbol?)
   (value expression?)
   (body expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (emptylist-exp)
  (null?-exp
   (body expression?)))
|#

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (pair-val
   (car expval?)
   (cdr expval?))
  (emptylist-val))

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

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

(define (expval->bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool v))))

(define (expval->pair v)
  (cases expval v
    (pair-val (car cdr) (cons car cdr))
    (else (expval-extractor-error 'pair v))))

(define (expval-car v)
  (cases expval v
    (pair-val (car cdr) car)
    (else (expval-extractor-error 'car v))))

(define (expval-cdr v)
  (cases expval v
    (pair-val (car cdr) cdr)
     (else (expval-extractor-error 'cdr v))))

(define (expval-null? v)
  (cases expval v
    (emptylist-val () (bool-val #t))
    (else (bool-val #f))))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors ": ~s, ~s" variant value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1) (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (if-exp (exp1 exp2 exp3)
            (let ([val1 (value-of exp1 env)])
              (if (expval->bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    (let-exp (var exp1 body)
             (let ([val1 (value-of exp1 env)])
               (value-of body (extend-env var val1 env))))
    (emptylist-exp ()
                   (emptylist-val))
    (cons-exp (exp1 exp2)
              (let ([val1 (value-of exp1 env)]
                    [val2 (value-of exp2 env)])
                (pair-val val1 val2)))
    (car-exp (exp1)
             (expval-car (value-of exp1 env)))
    (cdr-exp (exp1)
             (expval-cdr (value-of exp1 env)))
    (null?-exp (exp1)
               (expval-null? (value-of exp1 env)))
    (zero?-exp (exp1)
               (let* ([val1 (value-of exp1 env)]
                      [num (expval->num val1)])
                 (if (zero? num)
                     (bool-val #t)
                     (bool-val #f))))
    (equal?-exp (exp1 exp2)
                (let* ([val1 (value-of exp1 env)]
                       [val2 (value-of exp2 env)]
                       [num1 (expval->num val1)]
                       [num2 (expval->num val2)])
                  (bool-val (= num1 num2))))
    (less?-exp (exp1 exp2)
               (let* ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)]
                      [num1 (expval->num val1)]
                      [num2 (expval->num val2)])
                 (bool-val (< num1 num2))))
    (greater?-exp (exp1 exp2)
                  (let* ([val1 (value-of exp1 env)]
                         [val2 (value-of exp2 env)]
                         [num1 (expval->num val1)]
                         [num2 (expval->num val2)])
                    (bool-val (> num1 num2))))
    (diff-exp (exp1 exp2)
              (let* ([val1 (value-of exp1 env)]
                     [val2 (value-of exp2 env)]
                     [num1 (expval->num val1)]
                     [num2 (expval->num val2)])
                (num-val (- num1 num2))))
    (add-exp (exp1 exp2)
             (let* ([val1 (value-of exp1 env)]
                    [val2 (value-of exp2 env)]
                    [num1 (expval->num val1)]
                    [num2 (expval->num val2)])
               (num-val (+ num1 num2))))
    (mult-exp (exp1 exp2)
              (let* ([val1 (value-of exp1 env)]
                     [val2 (value-of exp2 env)]
                     [num1 (expval->num val1)]
                     [num2 (expval->num val2)])
                (num-val (* num1 num2))))
    (div-exp (exp1 exp2)
             (let* ([val1 (value-of exp1 env)]
                    [val2 (value-of exp2 env)]
                    [num1 (expval->num val1)]
                    [num2 (expval->num val2)])
               (num-val (/ num1 num2))))   
    (minus-exp (body-exp)
               (let ([val1 (value-of body-exp env)])
                 (let ([num (expval->num val1)])
                   (num-val (- 0 num)))))))
    

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))


;(run "/(*(1,2), *(10, 2))")
;(run "cons(1, 2)")
;(run "car (cons (1, 2))")
    
;(run "let x = 4 in cons(x,cons(cons(-(x, 1), emptylist), emptylist))"
