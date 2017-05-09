#lang racket/base

(define WordSize 16)
(define RegisterCount 16)
(define MicrocodeSize 256)
(define MicrocodeWordSize 32)

(define simulator-registers
  '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F))
(define simulator-vars
  (append '(MPC Read? Write? MAR MBR) simulator-registers))

(struct stepper (rr rs step!))

;; xxx contracts
(provide (all-defined-out))
