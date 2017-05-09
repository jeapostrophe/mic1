#lang racket/base
(require racket/match
         racket/list
         racket/runtime-path
         "lib.rkt"
         (prefix-in ll: "low-level.rkt")
         "simulator.rkt")
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
  (let ()
    (local-require (submod "mic1-test.rkt" test))
    (define sim (make-MIC1-simulator ll:make-MIC1-step FIB-MICRO-IMAGE empty 0 100))
    (debug-MIC1 sim)
    (define mem (simulator-mem sim))
    (local-require racket/vector)
    (chk (vector-copy mem 100 125)
         (vector 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377
                 610 987 1597 2584 4181 6765 10946 17711 28657 46368))))

(module+ main
  (require racket/cmdline)

  ;; xxx do something like this for testing
  #;(when (zero? (vector-length (current-command-line-arguments)))
      (current-command-line-arguments
       (vector (path->string standard-prom-path)
               (path->string example-asm-path))))

  (define InitialPC 0)
  (define InitialSP 1024)
  (command-line
   #:program "mic1"
   ;; xxx choose high or low-level emu
   #:once-each
   [("--pc") pc-str "Initial Program Counter (default: 0)"
    (set! InitialPC (string->number pc-str))]
   [("--sp") sp-str "Initial Stack Pointer (default: 1024)"
    (set! InitialSP (string->number sp-str))]
   #:args (microcode-path memory-image-path)

   (define start!
     (make-MIC1-simulator
      ll:make-MIC1-step
      (file->image microcode-path)
      (file->image memory-image-path)
      InitialPC
      InitialSP))
   (debug-MIC1 start!)))
