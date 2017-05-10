#lang racket/base
(require racket/match
         racket/list
         racket/runtime-path
         "lib.rkt"
         "simulator.rkt"
         (prefix-in ll: "low-level.rkt")
         (prefix-in hl: "high-level.rkt"))
(module+ test
  (require chk))

(define (debug-MIC1 s)
  (match-define (simulator Microcode Memory r! r start!) s)
  (let/ec esc
    (define cycle 0)
    (define subcycle 0)

    (local-require racket/format)
    (define (dump-state!)
      (define LABELW 10)
      (for ([rn (in-list simulator-vars)])
        (define x (r rn))
        (displayln
         (~a (~a #:min-width LABELW rn) " : "
             (~r x #:base 2 #:min-width 16 #:pad-string "0")
             " , or "
             (~r x #:min-width 6)
             " or signed "
             (~r (unsigned->signed 16 x) #:min-width 6 #:sign '++))))
      (newline)
      (displayln
       (~a (~a #:min-width LABELW "Cycles") cycle)))

    (define (inform!)
      (when (and (= (r 'Read?) 1) (= (r 'Write?) 1))
        (dump-state!)

        ;; xxx implement debugger
        (eprintf "Exiting simulator...\n")
        (esc))

      (set! subcycle (modulo (add1 subcycle) 4))
      (set! cycle (if (= subcycle 0) (add1 cycle) cycle)))

    (start! inform!)))

(define (lines->image lines)
  (define line 0)
  (for/list ([l (in-list lines)]
             #:unless (char=? #\# (string-ref l 0)))
    (set! line (add1 line))
    (define bits
      (for/list ([c (in-list (reverse (string->list l)))]
                 [col (in-naturals 1)])
        (match c
          [#\0 #f]
          [#\1 #t]
          [_ (error 'file->image "Illegal character on line ~a, col ~a: ~v"
                    line col c)])))
    (define n (bits->number bits))
    n))

(define (file->image p)
  (local-require racket/file)
  (lines->image (file->lines p)))

(module+ test
  (define (test-debugger-on-fib make-MIC1-step)
    (local-require (submod "mic1-test.rkt" test))
    (define sim (make-MIC1-simulator make-MIC1-step FIB-MICRO-IMAGE empty 0 100))
    (debug-MIC1 sim)
    (define mem (simulator-mem sim))
    (local-require racket/vector)
    (chk (vector-copy mem 100 125)
         (vector 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377
                 610 987 1597 2584 4181 6765 10946 17711 28657 46368)))

  (with-chk (['sim 'll])
    (test-debugger-on-fib ll:make-MIC1-step))
  (with-chk (['sim 'hl])
    (test-debugger-on-fib hl:make-MIC1-step)))

(define (main!)
  (local-require racket/cmdline)

  (define InitialPC 0)
  (define InitialSP 1024)
  (define make-MIC1-step ll:make-MIC1-step)
  (command-line
   #:program "mic1"
   #:once-any
   [("--ll") "Use low-level (gate based) simulator (default)"
    (set! make-MIC1-step ll:make-MIC1-step)]
   [("--hl") "Use high-level (algorithmic) simulator"
    (set! make-MIC1-step hl:make-MIC1-step)]
   #:once-each
   [("--pc") pc-str "Initial Program Counter (default: 0)"
    (set! InitialPC (string->number pc-str))]
   [("--sp") sp-str "Initial Stack Pointer (default: 1024)"
    (set! InitialSP (string->number sp-str))]
   #:args (microcode-path memory-image-path)

   (ll:compile-MIC1-circuit? #t)
   (define start!
     (make-MIC1-simulator
      make-MIC1-step
      (file->image microcode-path)
      (file->image memory-image-path)
      InitialPC
      InitialSP))
   (debug-MIC1 start!)))

(module+ test
  (require racket/runtime-path
           racket/string)
  (define-runtime-path standard-prom-path "../../examples/prom.dat")
  (define-runtime-path example-asm-path "../../examples/IO_str_and_echo.o")
  (define os (open-output-string))
  (parameterize ([current-command-line-arguments
                  (vector (path->string standard-prom-path)
                          (path->string example-asm-path))]
                 [current-input-port
                  (open-input-string "abcdefghijklmnopqrstuvwxyz\n0123456789\n")]
                 [current-output-port os])
    (main!))
  (match-define (list* sample in0 in1 more) (string-split (get-output-string os) "\n"))
  (chk sample "THIS IS A TEST STRING1\r"
       in0 "abcdefghijklmnopqrstuvwxyz"
       in1 "01\r"))

(module+ main
  (main!))

;; xxx add microcompiler
;; xxx add macroassembler
