#lang racket


(define (evaluate e env)
  (if (atom? e) ; (atom? e) == (not (pair? e))
    (cond [(symbol? e) (lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      [(quote)  (cadr e)]
      [(if)     (if (not (eq? (evaluate (cadr e) env) the-false-value))
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env))]
      [(begin)  (eprogn (cdr e) env)]
      [(set!)   (update! (cadr e) env (evaluate (caddr e) env))]
      [(lambda) (make-function (cadr e) (cddr e) env)]
      [else     (begin
                  (displayln "invoke...")
                  (displayln e)
                  (displayln "in environment...")
                  (displayln env)
                  (invoke (evaluate (car e) env)
                        (evlis (cdr e) env)))])))

(define the-false-value (cons "false" "boolean"))

(define (make-function variables body env)
  (lambda (values) 
    (eprogn body (extend env variables values))))


(define (invoke fn args) 
  (if (procedure? fn)
    (let [(result (fn args))]
      result)
    (error "Not a function" fn)))


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


(define empty-begin 813)
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


(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


(define env.init '())
(define env.global env.init)


(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (mcons 'name 'void) env.global))
            'name))
    ((definitial name value)
     (begin (set! env.global (cons (mcons 'name value) env.global))
            'name))))


(define-syntax defprimitive 
  (syntax-rules ()
    ((defprimitive name value)
     (definitial name 
        (lambda (values) 
              (apply value values))))))   

(define-syntax defpredicate
  (syntax-rules ()
    ((defpredicate name value)
     (definitial name
                 (lambda (values)
                   (or (apply value values) the-false-value))))))

(definitial t #t)
(definitial f the-false-value)
(definitial nil '())
(definitial foo 42)
(defprimitive list list)
(defprimitive + +)
(defpredicate = =)

(define (toplevel)
  (display (evaluate (read) env.global))
  (toplevel))


