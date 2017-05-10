#lang racket/base
(require racket/contract/base
         racket/match
         "lib.rkt"
         "hdl.rkt"
         "simulator.rkt")
(module+ test
  (require chk
           (submod "hdl.rkt" test)))

(define (MicroSeqLogic N Z Code Out)
  (Net (JumpOnN! JumpOnZ! NoJump JumpOnN JumpOnZ JumpAlways)
       (Decoder/N Code (list NoJump JumpOnN JumpOnZ JumpAlways))
       (And JumpOnN N JumpOnN!)
       (And JumpOnZ Z JumpOnZ!)
       (Or* (list JumpOnN! JumpOnZ! JumpAlways) Out)))
(module+ test
  (chk-tt MicroSeqLogic
          (for*/list ([N (in-range 2)] [Z (in-range 2)]
                      [C0 (in-range 2)] [C1 (in-range 2)])
            (define Code (+ C0 (* 2 C1)))
            (list (list N Z (list C0 C1))
                  (list
                   (if (or (and (= 1 N) (= Code 1))
                           (and (= 1 Z) (= Code 2))
                           (= Code 3))
                     1
                     0))))))

(define (ALU A B Function-Select Out Negative? Zero?)
  (define N (length A))
  (unless (= N (length B))
    (error 'ALU "B is wrong length"))
  (unless (= N (length Out))
    (error 'ALU "Out is wrong length"))
  (unless (= 2 (length Function-Select))
    (error 'ALU "Function Select is wrong length"))

  (Net ([TheSum N] [TheAnd N] [NotA N] [Function-Selects 4])
       (Adder/N A B FALSE GROUND TheSum)
       (And/N A B TheAnd)
       (Not/N A NotA)
       (Decoder/N Function-Select Function-Selects)
       (RegisterRead (list TheSum TheAnd A NotA) Function-Selects Out)
       (IsZero? Out Zero?)
       (IsNegative? Out Negative?)))
(module+ test
  (define-chk-num chk-alu
    #:N N
    #:in ([A N] [B N] [Function-Select 2])
    #:out ([Out N] Negative? Zero?)
    #:circuit ALU #:exhaust 5
    #:check
    (chk (vector Out Negative? Zero?)
         (let* ([Ans-premod
                 (match Function-Select
                   [0 (+ A B)]
                   [1 (bitwise-and A B)]
                   [2 A]
                   [3 (bitwise-not A)])]
                [Ans
                 (modulo Ans-premod (expt 2 N))])
           (vector Ans
                   (negative? (unsigned->signed N Ans))
                   (zero? Ans))))))

(define (MIC1 μCodeLength Microcode
              Registers MPC-out
              Read? Write?
              MAR MBR)
  (define μAddrSpace (ROM-AddrSpace Microcode))
  (define WordBits (length MBR))
  (define RegisterCount (length Registers))
  (define RegisterBits (integer-length (sub1 RegisterCount)))

  ;; Aliases
  (define MIR:RD Read?)
  (define MIR:WR Write?)
  (define-wires
    Clock:1 Clock:2 Clock:3 Clock:4
    N Z MicroSeqLogic-out
    [pre-MIR μCodeLength] [MIR μCodeLength]
    MIR:AMUX [MIR:COND 2] [MIR:ALU 2] [MIR:SH 2]
    MIR:MBR MBR? MIR:MAR MAR?
    MIR:ENC
    [MIR:C RegisterBits] [MIR:B RegisterBits] [MIR:A RegisterBits]
    [MIR:ADDR μAddrSpace]
    #;[MPC-out μAddrSpace] [Mmux-out μAddrSpace]
    MPC-Inc-carry [MPC-Inc-out μAddrSpace]
    [Asel RegisterCount] [Bsel RegisterCount] [Csel RegisterCount]

    [A-Bus WordBits] [B-Bus WordBits] [C-Bus WordBits]
    [A-latch-out WordBits] [B-latch-out WordBits]
    [Amux-out WordBits] [ALU-out WordBits]
    Shifter-Left? Shifter-Right? Write-C?)
  (Net ()
       (Clock (list Clock:1 Clock:2 Clock:3 Clock:4))
       (ROM Microcode MPC-out pre-MIR)
       (Latch/N Clock:1 pre-MIR MIR)
       (Cut/N MIR
              (reverse
               (list MIR:AMUX MIR:COND MIR:ALU MIR:SH
                     MIR:MBR MIR:MAR MIR:RD MIR:WR
                     MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR)))

       (Decoder/N MIR:A Asel)
       (Decoder/N MIR:B Bsel)
       (Decoder/N MIR:C Csel)
       (RegisterRead Registers Asel A-Bus)
       (RegisterRead Registers Bsel B-Bus)
       (Latch/N Clock:2 A-Bus A-latch-out)
       (Latch/N Clock:2 B-Bus B-latch-out)
       (And MIR:MAR Clock:3 MAR?)
       (Latch/N MAR? B-latch-out MAR)
       (Mux/N A-latch-out MBR MIR:AMUX Amux-out)
       (ALU Amux-out B-Bus MIR:ALU ALU-out N Z)
       (MicroSeqLogic N Z MIR:COND MicroSeqLogic-out)
       (Mux/N MPC-Inc-out MIR:ADDR MicroSeqLogic-out Mmux-out)
       (Decoder/N MIR:SH (list GROUND Shifter-Right? Shifter-Left? GROUND))
       (Shifter/N Shifter-Left? Shifter-Right? ALU-out C-Bus)
       (And MIR:MBR Clock:4 MBR?)
       (Latch/N MBR? C-Bus MBR)
       (And Clock:4 MIR:ENC Write-C?)
       (RegisterSet Write-C? C-Bus Csel Registers)
       (Latch/N Clock:4 Mmux-out MPC-out)
       (Increment/N MPC-out MPC-Inc-carry MPC-Inc-out)))

(define compile-MIC1-circuit? (make-parameter #f))

(define (make-MIC1-step MicrocodeVec)
  (define Microcode
    (vector->list MicrocodeVec))

  (define-wires
    [MPC (ROM-AddrSpace Microcode)]
    Read? Write?
    [MAR WordSize]
    [MBR WordSize])
  (define Registers
    (for/list ([reg (in-list simulator-registers)])
      (Bundle WordSize #:debug reg)))
  (define simulator-wires
    (list* MPC Read? Write? MAR MBR Registers))
  (define WireMap
    (for/hasheq ([label (in-list simulator-vars)]
                 [reg (in-list simulator-wires)])
      (values label reg)))
  (define (register-set! r n)
    (write-number! (hash-ref WireMap r) n))
  (define (register-read r)
    (read-number (hash-ref WireMap r)))

  (define the-mic1
    (MIC1 MicrocodeWordSize Microcode
          Registers MPC
          Read? Write?
          MAR MBR))

  (define real-simulate!
    (if (compile-MIC1-circuit?)
      (compile-simulate! the-mic1
                         #:label "MIC1"
                         #:visible-wires simulator-wires)
      (λ () (simulate! the-mic1))))

  (stepper register-read register-set!
           (λ ()
             ;; It takes four rounds of simulation for one machine
             ;; cycle
             (for ([subcycle (in-range 4)])
               (real-simulate!)))))

(provide
 (contract-out
  [compile-MIC1-circuit? (parameter/c boolean?)]
  [make-MIC1-step
   (-> (vectorof exact-integer?)
       stepper?)]))
