#lang racket/base
(require racket/match
         racket/contract/base
         "lib.rkt")

(define (12bit x) (modulo x (expt 2 12)))
(define (16bit x) (modulo x (expt 2 16)))

(define simulator-registers
  '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F))
(define simulator-vars
  (append '(MPC Read? Write? MAR MBR) simulator-registers))

(define WordSize 16)
(define RegisterCount (length simulator-registers))
(define MicrocodeSize 256)
(define MicrocodeWordSize 32)

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
  (r! 'N1 (sub1 (expt 2 WordSize)))
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

  (define UART-RX-D 4092)
  (define UART-RX-C 4093)
  (define UART-TX-D 4094)
  (define UART-TX-C 4095)

  (simulator
   MicrocodeVec Memory r! r
   (λ (inform!)
     (let loop ([readc 0] [writec 0])
       (step!)

       (define next-writec (if (= 1 (r 'Write?)) (add1 writec) 0))
       (when (= next-writec 2)
         (define addr (12bit (r 'MAR)))
         (define val (r 'MBR))
         (vector-set! Memory addr val)

         ;; UART: TX is on and done and data written, so emit and swap
         ;; D&B
         (when (and (= (vector-ref Memory UART-TX-C) #b1010)
                    (= addr UART-TX-D))
           (vector-set! Memory UART-TX-C #b1001)
           (write-byte (bitwise-bit-field val 0 8))
           (flush-output)
           (vector-set! Memory UART-TX-C #b1010))

         ;; UART: Stablize control bytes
         (define (uart-stabilize! c-addr default)
           (define c (vector-ref Memory c-addr))
           (when (and (bitwise-bit-set? c 3)
                      (not (bitwise-bit-set? c 0))
                      (not (bitwise-bit-set? c 1)))
             (vector-set! Memory c-addr default)))
         (uart-stabilize! UART-RX-C #b1001)
         (uart-stabilize! UART-TX-C #b1010)

         (set! next-writec 0))

       (define next-readc (if (= 1 (r 'Read?)) (add1 readc) 0))
       (when (= next-readc 2)
         (define addr (12bit (r 'MAR)))
         (r! 'MBR (vector-ref Memory addr))
         
         ;; UART: RX is on and done and data read, so swap D & B
         (when (and (= (vector-ref Memory UART-RX-C) #b1010)
                    (= addr UART-RX-D))
           (vector-set! Memory UART-RX-C #b1001))

         ;; UART: RX is on and busy, and there's a char, so read it
         ;; and enable D. This is in the read section because we
         ;; assume the program is reading memory to poll.
         (when (and (= (vector-ref Memory UART-RX-C) #b1001)
                    (byte-ready?))
           (vector-set! Memory UART-RX-D (read-byte))
           (vector-set! Memory UART-RX-C #b1010))

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

(define μinst/c (listof any/c))
(provide
 (contract-out
  [WordSize exact-integer?]
  [RegisterCount exact-integer?]
  [MicrocodeWordSize exact-integer?]
  [simulator-vars (listof symbol?)]
  [simulator-registers (listof symbol?)]
  [12bit (-> exact-integer? exact-integer?)]
  [16bit (-> exact-integer? exact-integer?)]
  [μencode (-> μinst/c (listof exact-integer?))]
  [μwrite (-> (listof exact-integer?) exact-integer?)]
  [μdecode (-> exact-integer? μinst/c)]
  (struct stepper ([rr (-> (apply or/c simulator-vars) exact-integer?)]
                   [rs (-> (apply or/c simulator-vars) exact-integer? void?)]
                   [step! (-> void?)]))
  (struct simulator ([mc (vectorof exact-integer?)]
                     [mem (vectorof exact-integer?)]
                     [rs (-> (apply or/c simulator-vars) exact-integer? void?)]
                     [rr (-> (apply or/c simulator-vars) exact-integer?)]
                     [start (-> (-> void?) any)]))
  [make-MIC1-simulator
   (-> (-> (vectorof exact-integer?) stepper?)
       (listof exact-integer?)
       (listof exact-integer?)
       exact-integer?
       exact-integer?
       simulator?)]))
