#lang racket
(provide (all-defined-out))

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))
;; given
; (define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
; (define mse (lambda (x y) (mul (sub x y) (sub x y))))
; (define get-value (lambda (x) (if (eq? '() x) (x) (car x) )))
(define get-value (lambda (x) (if (list? x) (map cadr x) (num-value x))))
(define get-grad (lambda (x) (if (list? x) (map caddr x) (num-grad x))))
(define add (lambda args (num (eval (cons + (map get-value args))) (eval(cons + (map get-grad args))))))
(define totalVal (lambda (x) (eval(cons * x))))
(define matcher (lambda (x y z) (if (eq? x '()) '() (cons (list (car x) (car y) z) (matcher (cdr x) (cdr y) z))) ))
(define calculator (lambda (x) (/ (* (cadr x) (caddr x)) (car x))))
(define general (lambda (x) (map calculator x)))
(define mul (lambda args (num (eval (cons * (map get-value args))) (eval (cons + (general (matcher (map get-value args) (map get-grad args) (totalVal (map get-value args)))))) )))
(define sub (lambda args (num (eval (cons - (map get-value args))) (eval(cons - (map get-grad args))))))
(define create-hash-helper (lambda (x y z) (if (eq? x '()) '()  (cons (cons (car x) (if (eq? z (car x)) (num (car y) 1.0) (num (car y) 0.0) ) ) (create-hash-helper (cdr x) (cdr y) z)))) )
(define create-hash (lambda (x y z) (make-hash (create-hash-helper x y z))))
(define parser (lambda (y x) (cond ((eq? x '()) '())  ( (list? x)  (cons (parser y (car x)) (parser y (cdr x))))  ((eq? x '+ ) 'add ) ((eq? x '- ) 'sub ) ((eq? x '* ) 'mul ) ((eq? x 'relu ) 'relu )
((eq? x 'mse ) 'mse ) ((number? x) (num x 0.0) )  (else (num (cadr (hash-ref y x)) (cddr (hash-ref y x))) ) ) ))   
