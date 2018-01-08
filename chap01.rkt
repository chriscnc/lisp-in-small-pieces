#lang racket


(define (evaluate e env)
  (if (atom? e) ; (atom? e) == (not (pair? e))
    (cond [(symbol? e) (lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (case (car e)
      ((quote)  (cadr e))
      ((if)     (if (evaluate (cadr e) env)
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
      ((begin)  (eprogn (cdr e) env))
      ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
      ((lambda) (make-function (cadr e) (cddr e) env))
      (else     (invoke (evaluate (car e) env)
                        (evlis (cdr e) env))))))


(define (eprogn exps env)
  (if (pair? exps)
    (if (pair? (cdr exps))
      (begin (evaluate (car exps) env)
             (eprogn (cdr exps) env))
      (evaluate (car exps) env))
    empty-begin))


(define (evlis exps env)
  (if (pair? exps)
    (cons (evaluate (car exps))
          (evlis (cdr exps env)))
    '()))

(define empty-begin 813)



(define (lookup e env)
  e)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
