#lang racket

(define hello-world ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
]<+.")

(define (new-context x) (list (make-vector x) 0))

(define (run-program code ctx) 
    (-run-program code ctx 0))

(define (-run-program code ctx count) 
    (cond 
        [(non-empty-string? code) 
            (define c (substring code 0 1))
            (define ode (substring code 1 (string-length code)))

            (cond 
                [(string=? c "]") (values ctx (+ count 1))]
                [(string=? c "[")
                    (define-values (iter_ctx iter_count) (-run-program ode ctx 0))
                    (define new_ode (substring ode iter_count (string-length ode)))
                    
                    (-run-program new_ode iter_ctx (+ 1 count iter_count))]
                [else
                    (define-values (new_ctx new_count) 
                        (if (valid-instructions c) 
                            (values (run-instruction c ctx) (+ count 1))
                            (values ctx count)))
                    
                    (-run-program ode new_ctx new_count)])]
        [else (values ctx count)]))

(define (valid-instructions c) (string-contains? "<>+-,." c))

(define (run-instruction i ctx) 
    (cond
        [(string=? i ">") (cell-next ctx)]
        [(string=? i "<") (cell-prev ctx)]
        [(string=? i "+") (cell-modify ctx (lambda (x) (+ x 1)))]
        [(string=? i "-") (cell-modify ctx (lambda (x) (- x 1)))]
        [(string=? i ",") (cell-modify ctx (lambda (_) (read-byte)))]
        [(string=? i ".") (cell-modify ctx (lambda (x) (printf "~a" (bytes->string/utf-8 (bytes x))) x))]))

(define (cell-next ctx) (list (car ctx) (+ (car (cdr ctx)) 1)))
(define (cell-prev ctx) (list (car ctx) (- (car (cdr ctx)) 1)))

(define (cell-modify ctx p)
    (define pos (car (cdr ctx)))
    (define vec (vector-copy (car ctx)))
    (define val (vector-ref vec pos))
    (define nv (p val))
    (vector-set! vec pos nv)
    (list vec pos))
