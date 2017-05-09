#lang racket/base
(require racket/match)

;; xxx implement same api as low-level

(struct μi (AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR) #:transparent)
(define REGISTERS
  (list->vector '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F)))
(define (reg-decode n)
  (vector-ref REGISTERS n))
(define (micro-decode n)
  (μi (bitwise-bit-set? n 32)
      (match (bitwise-bit-field n 30 32)
        [0 'NJ] [1 'JN] [2 'JZ] [3 'J!])
      (match (bitwise-bit-field n 28 30)
        [0 '+] [1 '&] [2 'A] [3 '!])
      (match (bitwise-bit-field n 26 28)
        [0 'NS] [1 'RS] [2 'LS]
        [3 (error 'micro-decode "Shift field may not be 11")])
      (bitwise-bit-set? n 25)
      (bitwise-bit-set? n 24)
      (bitwise-bit-set? n 23)
      (bitwise-bit-set? n 22)
      (bitwise-bit-set? n 21)
      (reg-decode (bitwise-bit-field 16 20))
      (reg-decode (bitwise-bit-field 12 16))
      (reg-decode (bitwise-bit-field 8 12))
      (bitwise-bit-field n 0 8)))
;; xxx test

(define (16bit x) (modulo x (expt 2 16)))
(define (MIC1-step! μrom st st!)  
  (define MPC (st 'MPC))
  (match-define
    (μi AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR)
    (μrom MPC))

  (st! 'Read? RD)
  (st! 'Write? WR)
  
  (define A-bus
    (if AMUX
      (st 'MBR)
      (st A)))
  (define B-bus (st B))
  (when MAR
    (st! 'MAR B-bus))

  (define ALU-fun
    (match ALU
      ['+ +]
      ['& bitwise-and]
      ['A (λ (a b) a)]
      ['! bitwise-not]))
  (define ALU-out
    (16bit (ALU-fun A-bus B-bus)))

  (define Z (zero? ALU-out))
  (define N (bitwise-bit-set? ALU-out 16))

  (define SH-out
    (16bit
     (arithmetic-shift
      ALU-out
      (match SH
        ['NS  0]
        ['RS -1]
        ['LS +1]))))

  (when MBR
    (st! 'MBR SH-out))
  (when ENC
    (st! C SH-out))

  (define micro-jump?
    (match COND
      ['NJ #f]
      ['JN N]
      ['JZ Z]
      ['J! #t]))
  (st! 'MPC
       (if micro-jump?
         ADDR
         (add1 MPC))))

(define-syntax-rule (while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))

(define (MIC1 microimage memoryimage initial-pc initial-sp)
  ;; Load microcode and memory
  (define microvec (make-vector 256 (micro-decode 0)))
  (for ([i (in-naturals)]
        [μn (in-list microimage)])
    (vector-set! microvec i (micro-decode μn)))

  (define memvec (make-vector (expt 2 12) 0))
  (for ([i (in-naturals)]
        [mn (in-list memoryimage)])
    (vector-set! memvec i mn))
  
  ;; Initialize State
  (define state (make-hasheq))
  (define (st r) (hash-ref state r))
  (define (st! r n) (hash-set! state r n))

  (st! 'Read? #f)
  (st! 'Write? #f)
  (st! 'PC initial-pc)
  (st! 'SP initial-sp)
  (st! 'Z 0)
  (st! 'P1 +1)
  (st! 'N1 -1)
  (st! 'AMASK #b0000111111111111)
  (st! 'SMASK #b0000000011111111)
  (for ([zr (in-list '(MPC MAR MBR AC IR TIR A B C D E F))])
    (st! zr 0))

  ;; Simulate
  (define (microcode a) (vector-ref microvec a))
  (let/ec esc
    (define read-count 0)
    (define write-count 0)
    (while #t
      (MIC1-step! microcode st st!)
      
      ;; xxx implement IO

      (when (and (st 'Read?) (st 'Write?))
        (esc))

      (set! read-count (+ read-count (if (st 'Read?) 1 0)))
      (when (= read-count 2)
        (st! 'MBR (vector-ref memvec (st 'MAR)))
        (set! read-count 0))
      
      (set! write-count (+ write-count (if (st 'Write?) 1 0)))
      (when (= write-count 2)
        (vector-set! memvec (st 'MAR) (st 'MBR))
        (set! write-count 0)))))

;; xxx test

;; xxx write main
