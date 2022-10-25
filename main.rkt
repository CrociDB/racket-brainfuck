#lang racket

(define hello-world ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
]<+.")

(define (new-context x) (list (make-vector x) 0))

(define (run-program code ctx) 
    (cond 
        [(non-empty-string? code) 
            (define c (substring code 0 1))
            (define ode (substring code 1 (string-length code)))
            (cond 
                [(valid-instructions c)
                 (run-program ode (run-instruction c ctx))]
                [else ctx])]
        [else ctx]))

(define (valid-instructions c) (string-contains? "<>+-[],." c))

(define (run-instruction i ctx) 
    (printf "Interpreting ~a\n" i)
    (cond
        [(string=? i ">") (cell-next ctx)]
        [(string=? i "<") (cell-prev ctx)]
        [(string=? i "+") (cell-modify ctx (lambda (x) (+ x 1)))]
        [(string=? i "-") (cell-modify ctx (lambda (x) (- x 1)))]))

(define (cell-next ctx) (list (car ctx) (+ (car (cdr ctx)) 1)))
(define (cell-prev ctx) (list (car ctx) (- (car (cdr ctx)) 1)))

(define (cell-modify ctx p)
    (define pos (car (cdr ctx)))
    (define vec (vector-copy (car ctx)))
    (define val (vector-ref vec pos))
    (define nv (p val))
    (vector-set! vec pos nv)
    (list vec pos))
