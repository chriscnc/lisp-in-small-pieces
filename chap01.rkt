#lang racket


(define (evaluate e env)
  (if (atom? e) ; (atom? e) == (not (pair? e))
    (cond [(symbol? e) (lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      [(quote)  (cadr e)]
      [(if)     (if (evaluate (cadr e) env)
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env))]
      [(begin)  (eprogn (cdr e) env)]
      [(set!)   (update! (cadr e) env (evaluate (caddr e) env))]
      [(lambda) (make-function (cadr e) (cddr e) env)]
      [else     (invoke (evaluate (car e) env)
                        (evlis (cdr e) env))]
      )))


(define (make-function variables body env)
  (lambda (values) 
    (eprogn body (extend env.init variables values))))


(define (extend env variables values)
  (cond [(pair? variables)
         (if (pair? values)
           (cons (mcons (car variables) (car values))
                 (extend env (cdr variables) (cdr values)))
           (error "Extend Env: Too few values"))]
        [(null? variables)
         (if (null? values)
           env
           (error "Extend Env: Too many values"))]
        [(symbol? variables)
         (cons (mcons variables values) env)]))


(define (invoke fn args) 
  (if (procedure? fn)
    (fn args)
    (error "Not a function" fn)))

(define (eprogn exps env)
  (if (pair? exps)
    (if (pair? (cdr exps))
      (begin (evaluate (car exps) env)
             (eprogn (cdr exps) env))
      (evaluate (car exps) env))
    empty-begin))


(define (evlis exps env)
  (if (pair? exps)
    (cons (evaluate (car exps) env)
          (evlis (cdr exps) env))
    '()))


(define empty-begin 813)


(define (lookup id env)
  (if (pair? env)
    (if (eq? (mcar (car env)) id)
      (mcdr (car env))
      (lookup id (cdr env)))
    (error "No such binding" id)))


(define (update! id env value)
  (if (pair? env)
    (if (eq? (mcar (car env)) id)
      (begin (set-mcdr! (car env) value)
             value)
      (update! id (cdr env) value))
    (error "No such binding" id)))


(define env.init '())





(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
