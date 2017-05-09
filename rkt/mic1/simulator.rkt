#lang racket/base
(require racket/match
         "lib.rkt")

(define WordSize 16)
(define RegisterCount 16)
(define MicrocodeSize 256)
(define MicrocodeWordSize 32)

(define (12bit x) (modulo x (expt 2 12)))
(define (16bit x) (modulo x (expt 2 16)))

(define simulator-registers
  '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F))
(define simulator-vars
  (append '(MPC Read? Write? MAR MBR) simulator-registers))

(struct stepper (rr rs step!))

(define (image->memory MemSize WordSize Image)
  (define Mem (make-vector MemSize 0))
  (define ImageLen (length Image))
  (unless (<= ImageLen MemSize)
    (error 'image->memory "MemoryImage is too large: ~v vs ~v"
           MemSize ImageLen))

  (for ([i (in-naturals)]
        [m (in-list Image)])
    (unless (<= (integer-length m) WordSize)
      (error 'image->memory "Image word ~a too large" i))
    (vector-set! Mem i m))

  Mem)

(struct simulator (mc mem rs rr start))

(define (make-MIC1-simulator
         make-MIC1-step
         MicrocodeImage MemoryImage InitialPC InitialSP)

  (define MicrocodeVec
    (image->memory MicrocodeSize MicrocodeWordSize MicrocodeImage))

  (match-define (stepper r r! step!) (make-MIC1-step MicrocodeVec))

  (r! 'MPC 0)
  (r! 'Read? 0)
  (r! 'Write? 0)
  (r! 'MAR 0)
  (r! 'MBR 0)
  (r! 'PC InitialPC)
  (r! 'AC 0)
  (r! 'SP InitialSP)
  (r! 'IR 0)
  (r! 'TIR 0)
  (r! 'Z 0)
  (r! 'P1 +1)
  (r! 'N1 -1)
  (r! 'AMASK #b0000111111111111)
  (r! 'SMASK #b0000000011111111)
  (r! 'A 0)
  (r! 'B 0)
  (r! 'C 0)
  (r! 'D 0)
  (r! 'E 0)
  (r! 'F 0)

  (define Memory
    ;; Image is smaller because there are 4 bits in instructions. This
    ;; could be removed with memory banking or by allowing the stack
    ;; to be higher, etc.
    (image->memory (expt 2 (- WordSize 4)) WordSize MemoryImage))

  (simulator
   MicrocodeVec Memory r! r
   (λ (inform!)
     (let loop ([readc 0] [writec 0])
       (step!)

       (define next-readc (if (= 1 (r 'Read?)) (add1 readc) 0))
       (define next-writec (if (= 1 (r 'Write?)) (add1 writec) 0))
       ;; xxx implement IO
       (when (= next-writec 2)
         (vector-set! Memory (12bit (r 'MAR)) (r 'MBR))
         (set! next-writec 0))
       (when (= next-readc 2)
         (r! 'MBR (vector-ref Memory (12bit (r 'MAR))))
         (set! next-readc 0))

       (inform!)
       (loop next-readc next-writec)))))

(define (μencode ss)
  (match-define (list AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR) ss)
  (define register
    (match-lambda
      ['PC 0] ['AC 1] ['SP 2] ['IR 3] ['TIR 4] ['Z 5] ['P1 6] ['N1 7]
      ['AMASK 8] ['SMASK 9] ['A 10] ['B 11] ['C 12] ['D 13] ['E 14] ['F 15]))
  (list (match AMUX
          ['A 0]
          ['MBR 1])
        (match COND
          ['NJ 0]
          ['JN 1]
          ['JZ 2]
          ['J! 3])
        (match ALU
          ['+ 0]
          ['& 1]
          ['A 2]
          ['! 3])
        (match SH
          ['NS 0]
          ['RS 1]
          ['LS 2])
        (match MBR
          ['NB 0]
          ['MBR 1])
        (match MAR
          ['NA 0]
          ['MAR 1])
        (match RD
          ['NR 0]
          ['RD 1])
        (match WR
          ['NW 0]
          ['WR 1])
        (match ENC
          ['NC 0]
          ['ENC 1])
        (register C)
        (register B)
        (register A)
        ADDR))

;; xxx rewrite this to use math
(define (μwrite ns)
  (local-require racket/format)
  (define (b n [w 1])
    (~r n #:base 2 #:pad-string "0" #:min-width w))
  (match-define (list AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR) ns)
  (define ss (list (b AMUX) (b COND 2) (b ALU 2) (b SH 2) (b MBR) (b MAR)
                   (b RD) (b WR) (b ENC) (b C 4) (b B 4) (b A 4) (b ADDR 8)))
  (define s (apply string-append ss))
  (define n (string->number s 2))
  n)

(define REGISTERS
  (list->vector '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F)))
(define (reg-decode n)
  (vector-ref REGISTERS n))
(define (μdecode n)
  (list (if (bitwise-bit-set? n 31) 'MBR 'A)
        (match (bitwise-bit-field n 29 31)
          [0 'NJ] [1 'JN] [2 'JZ] [3 'J!])
        (match (bitwise-bit-field n 27 29)
          [0 '+] [1 '&] [2 'A] [3 '!])
        (match (bitwise-bit-field n 25 27)
          [0 'NS] [1 'RS] [2 'LS]
          [3 (error 'micro-decode "Shift field may not be 11")])
        (if (bitwise-bit-set? n 24) 'MBR 'NB)
        (if (bitwise-bit-set? n 23) 'MAR 'NA)
        (if (bitwise-bit-set? n 22) 'RD 'NR)
        (if (bitwise-bit-set? n 21) 'WR 'NW)
        (if (bitwise-bit-set? n 20) 'ENC 'NC)
        (reg-decode (bitwise-bit-field n 16 20))
        (reg-decode (bitwise-bit-field n 12 16))
        (reg-decode (bitwise-bit-field n 8 12))
        (bitwise-bit-field n 0 8)))

;; xxx contracts
(provide (all-defined-out))
