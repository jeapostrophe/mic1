#lang racket/base
(require racket/contract/base
         racket/match
         "lib.rkt"
         "simulator.rkt")

(define (step! μrom st st!)
  (define MPC (st 'MPC))
  (match-define
    (list AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR)
    (μdecode (vector-ref μrom MPC)))

  (st! 'Read? (if (eq? 'RD RD) 1 0))
  (st! 'Write? (if (eq? 'WR WR) 1 0))

  (define A-bus
    (if (eq? 'MBR AMUX)
      (st 'MBR)
      (st A)))
  (define B-bus (st B))
  (when (eq? 'MAR MAR)
    (st! 'MAR B-bus))

  (define ALU-fun
    (match ALU
      ['+ +]
      ['& bitwise-and]
      ['A (λ (a b) a)]
      ['! (λ (a b) (bitwise-not a))]))
  (define ALU-out
    (16bit (ALU-fun A-bus B-bus)))

  (define Z (zero? ALU-out))
  (define N (negative? (unsigned->signed WordSize ALU-out)))

  (define SH-out
    (16bit
     (arithmetic-shift
      ALU-out
      (match SH
        ['NS  0]
        ['RS -1]
        ['LS +1]))))

  (when (eq? 'MBR MBR)
    (st! 'MBR SH-out))
  (when (eq? 'ENC ENC)
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

(define (make-MIC1-step MicrocodeVec)
  (define state (make-hasheq))
  (define (st r) (hash-ref state r))
  (define (st! r n) (hash-set! state r n))

  (stepper st st! (λ () (step! MicrocodeVec st st!))))

(provide
 (contract-out
  [make-MIC1-step
   (-> (vectorof exact-integer?)
       stepper?)]))
