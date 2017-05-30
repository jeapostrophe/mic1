#!/usr/bin/env racket
#lang racket/base
(require racket/match
         racket/list
         racket/path
         racket/runtime-path
         "lib.rkt"
         "simulator.rkt"
         (prefix-in ll: "low-level.rkt")
         (prefix-in hl: "high-level.rkt"))
(module+ test
  (require chk))

(define (debug-MIC1 debug? make-s)
  (local-require readline
                 readline/pread
                 racket/format)

  (define (make-cmd-repl char read cmds)
    (define prom (string->bytes/utf-8 (format "~a " char)))
    (define (cmd-repl)
      (define in (parameterize ([readline-prompt prom]) (read)))
      (define cmd (if (list? in) (first in) in))
      (match (hash-ref cmds cmd #f)
        [#f
         (eprintf "Unknown command: ~v\n" cmd)
         ;; xxx add docs
         (eprintf "Available commands: ~a\n" (hash-keys cmds))
         (cmd-repl)]
        [proc
         (proc in)]))
    cmd-repl)

  ;; xxx set up breakpoints, step a cycle, show address relative to
  ;; symbols, etc
  (define outer-debug-loop
    (make-cmd-repl
     "#" (λ () (if (not debug?) 'go (read)))
     (hasheq
      'g
      (λ (c)
        (define s (make-s))

        (match-define (simulator Microcode Memory r! r start!) s)
        (define cycle 0)

        (define (display-value w rn x)
          (displayln
           (~a (~a #:min-width w rn) " : "
               (~r x #:base 2 #:min-width 16 #:pad-string "0")
               " , or "
               (~r x #:min-width 6)
               " or signed "
               (~r (unsigned->signed 16 x) #:min-width 6 #:sign '++))))

        (define (dump-state!)
          (define LABELW 10)
          (for ([rn (in-list simulator-vars)])
            (display-value LABELW rn (r rn)))
          (newline)
          (displayln
           (~a (~a #:min-width LABELW "Cycles") cycle)))

        (define (do-inner-debug-loop esc)
          (define inner-debug-loop
            (make-cmd-repl
             "$" read
             (hasheq 'q (λ (c) (esc))
                     'c (λ (c) (void))
                     'm (λ (c)
                          (define-values (start end)
                            (match c
                              [(list 'm (? number? c))
                               (values c (add1 c))]
                              [(list 'm (? number? s) 'to (? number? e))
                               (values s (add1 e))]
                              [(list 'm (? number? s) (? number? c))
                               (values s (+ s c))]
                              [_ (values 0 0)]))
                          (for ([c (in-range (min start end) (max start end))])
                            (display-value 5 c (vector-ref Memory c)))
                          (inner-debug-loop))
                     'st (λ (c)
                           (dump-state!)
                           (inner-debug-loop)))))
          (inner-debug-loop))

        (let/ec esc
          (start!
           (λ ()
             (set! cycle (add1 cycle))

             (when (and (= (r 'Read?) 1) (= (r 'Write?) 1))
               (eprintf "HALT\n")
               (if debug?
                 (do-inner-debug-loop esc)
                 (esc))))))

        (cond
          [debug?
           (outer-debug-loop)]
          [else
           (dump-state!)
           (eprintf "Exiting simulator...\n")])))))

  (outer-debug-loop))

(module+ test
  (define (test-debugger-on-fib which make-MIC1-step)
    (local-require (submod "mic1-test.rkt" test))
    (printf "Testing fib program (~a)\n" which)
    (define sim (make-MIC1-simulator make-MIC1-step FIB-MICRO-IMAGE empty 0 100))
    (debug-MIC1 sim)
    (define mem (simulator-mem sim))
    (local-require racket/vector)
    (chk (vector-copy mem 100 125)
         (vector 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377
                 610 987 1597 2584 4181 6765 10946 17711 28657 46368)))

  (with-chk (['sim 'll])
    (test-debugger-on-fib "low-level" ll:make-MIC1-step))
  (with-chk (['sim 'll-compiled])
    (ll:compile-MIC1-circuit? #t)
    (test-debugger-on-fib "low-level (C)" ll:make-MIC1-step))
  (with-chk (['sim 'hl])
    (test-debugger-on-fib "high-level" hl:make-MIC1-step)))

(define (file->image p)
  (local-require racket/file)
  (lines->image (file->lines p)))

(define (file->microcode-image p)
  (local-require "mcc.rkt")
  (match (path-get-extension p)
    ;; xxx expose symtab and add dprom (prom doesn't for backwards compat)
    [#".mc" (microcode->microcode-image/file p)]
    [#".prom" (file->image p)]
    [x (error 'mic1 "Unknown microcode extension: ~v" x)]))

(define (file->memory-image p)
  (local-require "masm.rkt")
  (match (path-get-extension p)
    ;; xxx expose the symtab to debugger
    [#".s" (let-values ([(s i) (asm->symtab+image/file p)]) i)]
    [#".o" (file->image p)]
    [x (error 'mic1 "Unknown memory extension: ~v" x)]))

(define (main!)
  (local-require racket/cmdline
                 raco/command-name)

  (define InitialPC 0)
  (define InitialSP 1024)
  (define Debug? #f)
  (define make-MIC1-step hl:make-MIC1-step)
  (command-line
   #:program (short-program+command-name)
   #:once-any
   [("--ll") "Use low-level (gate based) simulator with compiled CPU"
    (set! make-MIC1-step ll:make-MIC1-step)
    (ll:compile-MIC1-circuit? "MIC1")]
   [("--lli") "Use low-level (gate based) simulator with interpreted CPU"
    (set! make-MIC1-step ll:make-MIC1-step)
    (ll:compile-MIC1-circuit? #f)]
   [("--hl") "Use high-level (algorithmic) simulator (default)"
    (set! make-MIC1-step hl:make-MIC1-step)]
   #:once-each
   [("-d") "Run with debugger"
    (set! Debug? #t)]
   [("--pc") pc-str "Initial Program Counter (default: 0)"
    (set! InitialPC (string->number pc-str))]
   [("--sp") sp-str "Initial Stack Pointer (default: 1024)"
    (set! InitialSP (string->number sp-str))]
   #:usage-help "microcode-path must be either .prom (compiled) or .mc (source)"
   #:usage-help "memory-image-path must be either .o (compiled) or .s (source)"
   #:args (microcode-path memory-image-path)

   (debug-MIC1
    Debug?
    (λ ()
      (make-MIC1-simulator
       make-MIC1-step
       (file->microcode-image microcode-path)
       (file->memory-image memory-image-path)
       InitialPC
       InitialSP)))))

(module+ main
  (main!))
