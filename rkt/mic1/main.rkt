#lang racket/base
(require racket/match
         racket/list
         racket/runtime-path
         "lib.rkt"
         "low-level.rkt"
         "simulator.rkt")
(module+ test
  (require chk))

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

(define (make-MIC1-simulator
         MicrocodeImage MemoryImage InitialPC InitialSP)

  (define MicrocodeVec
    (image->memory MicrocodeSize MicrocodeWordSize MicrocodeImage))

  (match-define (stepper r r! step!) (make-MIC1-step MicrocodeVec))

  (r! 'PC InitialPC)
  (r! 'SP InitialSP)
  (r! 'Z 0)
  (r! 'P1 +1)
  (r! 'N1 -1)
  (r! 'AMASK #b0000111111111111)
  (r! 'SMASK #b0000000011111111)

  (define Memory
    ;; Image is smaller because there are 4 bits in instructions. This
    ;; could be removed with memory banking or by allowing the stack
    ;; to be higher, etc.
    (image->memory (expt 2 (- WordSize 4)) WordSize MemoryImage))

  (define (12bit x) (modulo x (expt 2 12)))

  (simulator
   MicrocodeVec Memory r! r
   (λ (inform!)
     (let loop ([readc 0] [writec 0])
       (step!)

       (define next-readc (if (= 1 (r 'Read?)) (add1 readc) 0))
       (define next-writec (if (= 1 (r 'Write?)) (add1 writec) 0))
       ;; xxx implement IO
       (when (> next-writec 4)
         (vector-set! Memory (12bit (r 'MAR)) (r 'MBR))
         (set! next-writec 0))
       (when (> next-readc 4)
         (r! 'MBR (vector-ref Memory (12bit (r 'MAR))))
         (set! next-readc 0))

       (inform!)
       (loop next-readc next-writec)))))

(struct simulator (mc mem rs rr start))

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

(module+ test
  (define (chk-μenc se en)
    (define s (μencode se))
    (define an (μwrite s))
    (with-chk (['mode 'μenc]
               ['se se]
               ['s s]
               ['an (number->string an 2)]
               ['en (number->string en 2)])
      (chk an en)))

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
    (chk (lines->image FIB-MICRO-IMAGE-STR) FIB-MICRO-IMAGE)
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
        (chk (number->string actual-n 2)
             (number->string expected 2))))))

(module+ test
  (define standard-reg-values
    (for/hasheq ([rn (in-list '(PC AC SP IR TIR A B C D E F))]
                 [v (in-naturals 10)])
      (values rn v)))

  (define (chk-mic1μs μinst-syms mem before . afters)
    (define μinst-ns (map μencode μinst-syms))
    (define μinst (map μwrite μinst-ns))
    (define s (make-MIC1-simulator μinst mem 0 1024))
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

      (define c 0)
      (define (inform!)
        (set! c (add1 c))
        (when (= c 4)
          (run-next-test!)
          (set! c 0)))
      (start! inform!)))

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
               (hasheq 'MPC 8))))

(module+ test
  #;(chk-mic1μs μinst-syms mem before (cons reg-after mem-after) ...)

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
    (define sim (make-MIC1-simulator FIB-MICRO-IMAGE empty 0 100))
    (debug-MIC1 sim)
    (define mem (simulator-mem sim))
    (local-require racket/vector)
    (chk (vector-copy mem 100 125)
         (vector 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377
                 610 987 1597 2584 4181 6765 10946 17711 28657 46368))))

(module+ main
  (require racket/cmdline)

  ;; xxx move this to testing system
  (define-runtime-path standard-prom-path
    "../../examples/prom.dat")

  (define-runtime-path example-asm-path
    "../../examples/adder.o")
  (when (zero? (vector-length (current-command-line-arguments)))
    (current-command-line-arguments
     (vector (path->string standard-prom-path)
             (path->string example-asm-path))))

  (define InitialPC 0)
  (define InitialSP 1024)
  (command-line
   #:program "mic1"
   #:once-each
   [("--pc") pc-str "Initial Program Counter (default: 0)"
    (set! InitialPC (string->number pc-str))]
   [("--sp") sp-str "Initial Stack Pointer (default: 1024)"
    (set! InitialSP (string->number sp-str))]
   #:args (microcode-path memory-image-path)

   (define start!
     (make-MIC1-simulator
      (file->image microcode-path)
      (file->image memory-image-path)
      InitialPC
      InitialSP))
   (debug-MIC1 start!)))
