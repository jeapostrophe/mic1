#lang racket/base
(require racket/match
         racket/list
         "simulator.rkt"
         (prefix-in ll: "low-level.rkt")
         (prefix-in hl: "high-level.rkt"))
(module+ test
  (require chk))

;; Encoding
(module+ test
  (define (chk-μdec an es)
    (define as (μdecode an))
    (with-chk (['mode 'μdec]
               ['es es]
               ['an (number->string an 2)])
      (chk as es)))
  (define (chk-μenc se en)
    (define s (μencode se))
    (define an (μwrite s))
    (with-chk (['mode 'μenc]
               ['se se]
               ['s s]
               ['an (number->string an 2)]
               ['en (number->string en 2)])
      (chk an en)))
  
  ;; MCC: ac := ac + mbr; goto START; 
  (chk-μdec #b11100000000100010000000100000000
            ;; This is bad because B=PC and A=MBR, so the op is ac := mbr + pc
            '(MBR J! + NS NB NA NR NW ENC AC PC AC 0))
  ;; MCC: ac := mbr + ac; goto START; 
  (chk-μdec #b11100000000100010001000000000000
            '(MBR J! + NS NB NA NR NW ENC AC AC PC 0))
  (chk-μenc '(MBR J! + NS NB NA NR NW ENC AC AC PC 0)
            #b11100000000100010001000000000000)

  (define FIB-MICRO-PROGRAM
    '(;; START/0: mar := sp; d := (1) + sp; wr
      (A NJ + NS NB MAR NR WR ENC D SP P1 0)
      ;; 1: f = (1) + (1); wr
      (A NJ + NS NB NA NR WR ENC F P1 P1 0)
      ;; 2: mbr := (1); b := (1); mar := d; wr;
      (A NJ A NS MBR MAR NR WR ENC B D P1 0)
      ;; 3: e := f + (1); wr;
      (A NJ + NS NB NA NR WR ENC E F P1 0)
      ;; LOOP/4: a := b + a;
      (A NJ + NS NB NA NR NW ENC A A B 0)
      ;; 5: d := sp + f;
      (A NJ + NS NB NA NR NW ENC D F SP 0)
      ;; 6: mar :=  d; mbr := a; wr; if n then goto DONE;
      (A JN A NS MBR MAR NR WR NC PC D A 11)
      ;; 7: sp := e + sp; wr;
      (A NJ + NS NB NA NR WR ENC SP SP E 0)
      ;; 8: b := a + b;
      (A NJ + NS NB NA NR NW ENC B B A 0)
      ;; 9: mar := sp; mbr := b; wr; if n then goto DONE;
      (A JN A NS MBR MAR NR WR NC PC SP B 11)
      ;; 10: sp := d; goto LOOP; wr;
      (A J! A NS NB NA NR WR ENC SP PC D 4)
      ;; DONE/11: wr; rd; goto START;
      (A J! + NS NB NA RD WR NC PC PC PC 0)))

  (define FIB-MICRO-IMAGE
    '(#b00000000101111010010011000000000
      #b00000000001111110110011000000000
      #b00010001101110111101011000000000
      #b00000000001111101111011000000000
      #b00000000000110101010101100000000
      #b00000000000111011111001000000000
      #b00110001101000001101101000001011
      #b00000000001100100010111000000000
      #b00000000000110111011101000000000
      #b00110001101000000010101100001011
      #b01110000001100100000110100000100
      #b01100000011000000000000000000000))

  (define FIB-MICRO-IMAGE-STR
    '("00000000101111010010011000000000"
      "00000000001111110110011000000000"
      "00010001101110111101011000000000"
      "00000000001111101111011000000000"
      "00000000000110101010101100000000"
      "00000000000111011111001000000000"
      "00110001101000001101101000001011"
      "00000000001100100010111000000000"
      "00000000000110111011101000000000"
      "00110001101000000010101100001011"
      "01110000001100100000110100000100"
      "01100000011000000000000000000000"))

  (with-chk (['mode "fib program encoding"])
    (chk (map (λ (s) (string->number s 2)) FIB-MICRO-IMAGE-STR)
         FIB-MICRO-IMAGE)
    (for ([expected FIB-MICRO-IMAGE]
          [actual-se (in-list FIB-MICRO-PROGRAM)]
          [i (in-naturals)])
      (define actual-ns (μencode actual-se))
      (define actual-n (μwrite actual-ns))
      (with-chk (['actual-se actual-se]
                 ['actual-ns actual-ns]
                 ['actual-n actual-n]
                 ['expected expected]
                 ['i i])
        (chk (number->string actual-n 2) (number->string expected 2)
             (μdecode actual-n) actual-se)))))

(module+ test
  (define standard-reg-values
    (for/hasheq ([rn (in-list '(PC AC SP IR TIR A B C D E F))]
                 [v (in-naturals 10)])
      (values rn v)))

  (define (run-all-mic1-tests make-MIC1-step)
    (define (chk-mic1μs μinst-syms mem before . afters)
      (define μinst-ns (map μencode μinst-syms))
      (define μinst (map μwrite μinst-ns))
      (define s (make-MIC1-simulator make-MIC1-step μinst mem 0 1024))
      (match-define (simulator Microcode Memory r! r start!) s)

      (for ([(rn n) (in-hash standard-reg-values)])
        (r! rn n))

      (for ([(rn n) (in-hash before)])
        (r! rn n))

      (define (make-init)
        (for/hasheq ([sv (in-list simulator-vars)])
          (values sv (r sv))))

      (let/ec esc
        (define afteri 0)
        (define (run-next-test!)
          (match-define (cons regs*mems more-afters) afters)
          (set! afters more-afters)
          (match-define (cons reg-after mem-after) regs*mems)
          (define init (make-init))

          (with-chk (['afteri afteri]
                     ['μinst-syms μinst-syms]
                     ['before before])
            ;; Everything not in after stayed the same
            (for ([(sv svb) (in-hash init)]
                  #:unless (hash-has-key? reg-after sv))
              (with-chk (['sv sv]
                         ['svb svb])
                (chk (r sv) svb)))
            ;; Everything in after got the new value
            (for ([(rn n) (in-hash reg-after)])
              (with-chk (['rn rn])
                (chk (r rn) n)))
            ;; Everything in memory has appropriate value
            (for ([(memi n) (in-hash mem-after)])
              (with-chk (['memi memi])
                (chk (vector-ref Memory memi) n))))

          (set! afteri (add1 afteri))

          (when (empty? afters)
            (esc)))

        (start! run-next-test!)))

    (define (chk-mic1μ μinst-sym before after)
      (chk-mic1μs (list μinst-sym) empty before (cons after (hasheq))))

    ;; Doesn't require the ALU to work correctly
    (with-chk (['mode 'RD])
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'RD 'NW 'NC 'PC 'PC 'PC 0)
                 (hasheq 'Read? 0)
                 (hasheq 'MPC 1 'Read? 1)))
    (with-chk (['mode 'WR])
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'WR 'NC 'PC 'PC 'PC 0)
                 (hasheq 'Write? 0)
                 (hasheq 'MPC 1 'Write? 1)))

    ;; Test MAR assignment (pre ALU)
    (with-chk (['mode 'MAR])
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'MAR 'NR 'NW 'NC 'PC 'A 'PC 0)
                 (hasheq 'A 88 'PC 1)
                 (hasheq 'MPC 1 'MAR 88)))

    ;; Test basic ALU operation
    (with-chk (['mode 'ALU])
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 3)
                 (hasheq 'MPC 1 'C 4))
      (chk-mic1μ (list 'A 'NJ '& 'NS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 3)
                 (hasheq 'MPC 1 'C 1))
      (chk-mic1μ (list 'A 'NJ 'A 'NS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 3)
                 (hasheq 'MPC 1 'C 3))
      (chk-mic1μ (list 'A 'NJ '! 'NS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 3)
                 (hasheq 'MPC 1 'C (modulo (bitwise-not 3) (expt 2 16)))))

    ;; Vary A side input
    (with-chk (['mode 'AMUX])
      ;; PC = Z(0) + A(-1)
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'ENC 'PC 'Z 'N1 0)
                 (hasheq 'MBR 30)
                 (hasheq 'MPC 1 'PC (modulo -1 (expt 2 16))))

      ;; PC = Z(0) + MBR(30)
      (chk-mic1μ (list 'MBR 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'ENC 'PC 'Z 'N1 0)
                 (hasheq 'MBR 30)
                 (hasheq 'MPC 1 'PC 30)))

    ;; Test shifting of ALU output
    (with-chk (['mode 'SH])
      ;; C = B(1) + A(1)
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 1)
                 (hasheq 'MPC 1 'C 2))
      ;; C = B(1) + A(1) >> 1
      (chk-mic1μ (list 'A 'NJ '+ 'RS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 1)
                 (hasheq 'MPC 1 'C 1))
      ;; C = B(1) + A(1) << 1
      (chk-mic1μ (list 'A 'NJ '+ 'LS 'NB 'NA 'NR 'NW 'ENC 'C 'B 'A 0)
                 (hasheq 'C 0 'B 1 'A 1)
                 (hasheq 'MPC 1 'C 4)))

    ;; Test where to assign ALU output
    (with-chk (['mode 'MBR])
      ;; MBR = PC + A
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'MBR 'NA 'NR 'NW 'NC 'PC 'A 'PC 0)
                 (hasheq 'PC 1 'A 88)
                 (hasheq 'MPC 1 'MBR 89)))

    (with-chk (['mode 'REGS])
      (define (do-regs-tests Cs Bs As)
        (for* ([C (in-list Cs)]
               [B (in-list Bs)]
               [A (in-list As)]
               #:unless (eq? C B)
               #:unless (eq? C A)
               #:unless (eq? A B))
          (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'ENC C B A 0)
                     (hasheq C 100 B 101 A 102)
                     (hasheq 'MPC 1 C 203))))
      (do-regs-tests simulator-registers '(A C) '(B D))
      (do-regs-tests '(A C) simulator-registers '(B D))
      (do-regs-tests '(A C) '(B D) simulator-registers))

    ;; Testing conditional output of ALU independent of destination
    (with-chk (['mode 'COND])
      ;; Don't jump with addr, still goes to next
      (chk-mic1μ (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'PC 'PC 8)
                 (hasheq)
                 (hasheq 'MPC 1))
      ;; Adding 0 + (-1) during JN goes to 8
      (chk-mic1μ (list 'A 'JN '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'Z 'N1 8)
                 (hasheq)
                 (hasheq 'MPC 8))
      ;; Adding 0 + (+1) during JN goes to 1
      (chk-mic1μ (list 'A 'JN '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'Z 'P1 8)
                 (hasheq)
                 (hasheq 'MPC 1))
      ;; Adding 0 + 0 during JZ goes to 8
      (chk-mic1μ (list 'A 'JZ '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'Z 'Z 8)
                 (hasheq)
                 (hasheq 'MPC 8))
      ;; Adding 0 + (+1) during JZ goes to 1
      (chk-mic1μ (list 'A 'JZ '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'Z 'P1 8)
                 (hasheq)
                 (hasheq 'MPC 1))
      ;; Using J! always goes to 8
      (chk-mic1μ (list 'A 'J! '+ 'NS 'NB 'NA 'NR 'NW 'NC 'PC 'PC 'PC 8)
                 (hasheq)
                 (hasheq 'MPC 8)))

    ;; Test memory read
    (chk-mic1μs (list (list 'A 'NJ '+ 'NS 'NB 'MAR 'RD 'NW 'NC 'PC 'A 'PC 0)
                      (list 'A 'NJ '+ 'NS 'NB 'NA 'RD 'NW 'NC 'PC 'A 'PC 0))
                (list 67 68)
                (hasheq 'A 1 'PC 1)
                (cons (hasheq 'MPC 1 'Read? 1 'MAR 1) (hasheq))
                (cons (hasheq 'MPC 2 'Read? 1 'MBR 68) (hasheq)))

    ;; Test memory write
    (chk-mic1μs (list (list 'A 'NJ 'A 'NS 'MBR 'MAR 'NR 'WR 'NC 'PC 'A 'PC 0)
                      (list 'A 'NJ '+ 'NS 'NB 'NA 'NR 'WR 'NC 'PC 'A 'PC 0))
                (list 67 68)
                (hasheq 'A 1 'PC 88)
                (cons (hasheq 'MPC 1 'Write? 1 'MAR 1 'MBR 88) (hasheq 1 68))
                (cons (hasheq 'MPC 2 'Write? 1) (hasheq 1 88)))

    ;; Test Fibonacci program
    (chk-mic1μs
     FIB-MICRO-PROGRAM
     empty (hasheq 'SP 0 'A 0)
     ;; Start
     (cons (hasheq 'MPC 1 'Write? 1 'MAR 0 'D 1) (hasheq))
     (cons (hasheq 'MPC 2 'Write? 1 'F 2) (hasheq 0 0))
     (cons (hasheq 'MPC 3 'Write? 1 'MBR 1 'B 1 'MAR 1) (hasheq 0 0))
     (cons (hasheq 'MPC 4 'Write? 1 'E 3) (hasheq 0 0 1 1))
     ;; Loop (iteration 1)
     (cons (hasheq 'MPC 5 'A 1) (hasheq 0 0 1 1))
     (cons (hasheq 'MPC 6 'D 2) (hasheq 0 0 1 1))
     (cons (hasheq 'MPC 7 'MAR 2 'MBR 1 'Write? 1) (hasheq 0 0 1 1))
     (cons (hasheq 'MPC 8 'SP 3 'Write? 1) (hasheq 0 0 1 1 2 1))
     (cons (hasheq 'MPC 9 'B 2) (hasheq 0 0 1 1 2 1))
     (cons (hasheq 'MPC 10 'MAR 3 'MBR 2 'Write? 1) (hasheq 0 0 1 1 2 1))
     (cons (hasheq 'MPC 4 'SP 2 'Write? 1) (hasheq 0 0 1 1 2 1 3 2))
     ;; Loop (iteration 2)
     (cons (hasheq 'MPC 5 'A 3) (hasheq 0 0 1 1 2 1 3 2))
     (cons (hasheq 'MPC 6 'D 4) (hasheq 0 0 1 1 2 1 3 2))
     (cons (hasheq 'MPC 7 'MAR 4 'MBR 3 'Write? 1)
           (hasheq 0 0 1 1 2 1 3 2))
     (cons (hasheq 'MPC 8 'SP 5 'Write? 1)
           (hasheq 0 0 1 1 2 1 3 2 4 3))))

  (with-chk (['sim 'll])
    (run-all-mic1-tests ll:make-MIC1-step))
  (with-chk (['sim 'hl])
    (run-all-mic1-tests hl:make-MIC1-step)))

(module+ test
  (provide FIB-MICRO-IMAGE))
