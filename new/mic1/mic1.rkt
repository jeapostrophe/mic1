#!/usr/bin/env racket
#lang racket/base
(require racket/match
         racket/list
         racket/path
         racket/runtime-path
         ;; xxx make private and move this whole directory out one (& make package)
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
    [#".mc" (microcode->microcode-image/file p)]
    [#".prom" (file->image p)]
    [x (error 'mic1 "Unknown microcode extension: ~v" x)]))

(define (file->memory-image p)
  (local-require "masm.rkt")
  (match (path-get-extension p)
    [#".s" (let-values ([(s i) (asm->symtab+image/file p)]) i)]
    [#".o" (file->image p)]
    [x (error 'mic1 "Unknown memory extension: ~v" x)]))

(define (main!)
  (local-require racket/cmdline)

  (define InitialPC 0)
  (define InitialSP 1024)
  (define make-MIC1-step hl:make-MIC1-step)
  (command-line
   #:program "mic1"
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
   [("--pc") pc-str "Initial Program Counter (default: 0)"
    (set! InitialPC (string->number pc-str))]
   [("--sp") sp-str "Initial Stack Pointer (default: 1024)"
    (set! InitialSP (string->number sp-str))]
   #:usage-help "microcode-path must be either .prom (compiled) or .mc (source)"
   #:usage-help "memory-image-path must be either .o (compiled) or .s (source)"
   #:args (microcode-path memory-image-path)

   (define start!
     (make-MIC1-simulator
      make-MIC1-step
      (file->microcode-image microcode-path)
      (file->memory-image memory-image-path)
      InitialPC
      InitialSP))
   (debug-MIC1 start!)))

(module+ main
  (main!))
