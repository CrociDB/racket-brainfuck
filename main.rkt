#lang racket

(define hello-world ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
]<+.")

(define (new-context x) (list (make-vector x) 0))

(define (run-program code ctx) 
    (when (non-empty-string? code) 
        (define i (substring code 0 1))
        (define ode (substring code 1 (string-length code)))
        (when (valid-instructions i)
            (run-program ode (run-instruction i ctx)))))

(define (valid-instructions c) (string-contains? "<>+_[],." c))

(define (run-instruction i ctx) 
    (printf "Interpreting ~a\n" i)
    (cond
        [(string=? i ">") (cell-next ctx)]
        [(string=? i "<") (cell-prev ctx)]))
        ; [(= i "+") (inc-cell ctx)]
        ; [(= i "-") (dec-cell ctx)]
        ; [(= i ".") (print-cell ctx)]
        ; [(= i ",") (read-cell ctx)]))

(define (cell-next ctx) (list (car ctx) (+ (car (cdr ctx)) 1)))
(define (cell-prev ctx) (list (car ctx) (- (car (cdr ctx)) 1)))

(define (modify-cell ctx p)
    (define pos (car (cdr ctx)))
    (define vec (vector-copy (car ctx)))
    (define val (vector-ref vec pos))
    (define nv (p val))
    (vector-set! vec pos nv)
    (list vec pos))