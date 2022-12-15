#lang racket

(require racket/cmdline)

; Brainfuck Implementation

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
                    (define-values (iter_ctx iter_count) (-run-subroutine ode ctx))
                    (define new_ode (substring ode iter_count (string-length ode)))
                    
                    (-run-program new_ode iter_ctx (+ 1 count iter_count))]
                [else
                    (define-values (new_ctx new_count) 
                        (if (valid-instructions c) 
                            (values (run-instruction c ctx) (+ count 1))
                            (values ctx count)))
                    
                    (-run-program ode new_ctx new_count)])]
        [else (values ctx count)]))

(define (-run-subroutine code ctx)
    (cond 
        [(cell-zero ctx)
            (values ctx (count-subroutine code 0 1))]
        [else 
            (define-values (iter_ctx iter_count) (-run-program code ctx 0))
            (-run-subroutine code iter_ctx)]))

(define (count-subroutine code count brackets)
    (cond 
        [(non-empty-string? code)
            (define c (substring code 0 1))
            (define ode (substring code 1 (string-length code)))

            (define new_brackets (cond
                [(string=? c "[") (+ brackets 1)]
                [(string=? c "]") (- brackets 1)]
                [else brackets]))

            (if (or (= new_brackets 0))
                (+ count 1)
                (count-subroutine ode (+ count 1) new_brackets))]
        [else count]))

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

(define (cell-zero ctx)
    (= 0 (vector-ref (car ctx) (car (cdr ctx)))))

; Command Line and Execution

(define num-cells 100)

(define file-to-run
  (command-line
   #:program "brainfuck interpreter"
   #:once-any
   [("-c" "--num-cells") c
                         "set the number of cells. the default is 100"
                         (set! num-cells (string->number c))]
   #:args (filename)
   filename))

(define code (file->string file-to-run))
(define-values (context _) (run-program code (new-context num-cells)))
