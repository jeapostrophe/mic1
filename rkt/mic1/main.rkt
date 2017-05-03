#lang racket/base
(require racket/match
         racket/list
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require chk
           (for-syntax racket/base
                       syntax/parse)))

;; Lib
(define (snoc l x) (append l (list x)))

;; Core
(struct wire (value))

(struct Nand (a b o))
;; xxx remove and use SR-latch?
(struct latch (latch? prev next))
;; xxx remove and use cell with a cycle and increment?
(struct clock (i os))

;; Constructors
(define TRUE (wire (box-immutable #t)))
(define FALSE (wire (box-immutable #f)))

(define Mt null)
(begin-for-syntax
  (define-syntax-class wire-spec
    #:attributes (i d)
    (pattern i:id
             #:attr d (syntax/loc #'i (Wire)))
    (pattern [i:id n:expr]
             #:attr d (syntax/loc #'i (Bundle n)))))
(define-simple-macro (define-wires w:wire-spec ...)
  (begin (define w.i w.d) ...))
(define-simple-macro (Net (w:wire-spec ...) b ...)
  (let ()
    (define-wires w ...)
    (list* b ...)))
(define (Cell) (latch TRUE (box #f) (box #f)))

(define (Wire) (wire (box #f)))
(define (Bundle n)
  (for/list ([i (in-range n)])
    (Wire)))

(define (Clock . os) (clock (box 0) os))

;; Simulator
(define bread
  (match-lambda
    [(wire vb) (unbox vb)]
    [(latch _ pb n) (unbox pb)]))
(define (bwrite! b ?)
  (match b
    [(wire vb)
     (set-box! vb ?)]
    [(latch latch? _ nb)
     (when (bread latch?)
       (set-box! nb ?))]))

(define (write-number! B n)
  (define len (integer-length n))
  (unless (<= len (length B))
    (error 'write-number! "Not enough bits for number"))
  (for ([b (in-list B)]
        [i (in-naturals)])
    (bwrite! b (bitwise-bit-set? n i))))
(define (read-number B)
  (for/fold ([n 0]) ([b (in-list B)] [i (in-naturals)])
    (+ n (* (if (bread b) 1 0) (expt 2 i)))))

(define (tree-walk n f)
  (match n
    [(cons a d)
     (tree-walk a f)
     (tree-walk d f)]
    [(or #f '() (? void?))
     (void)]
    [x
     (f x)]))

(define (net-count sn)
  (define C 0)
  (tree-walk sn (λ (_) (set! C (add1 C))))
  C)

(define (simulate! sn)
  (define ustep
    (match-lambda
      [(Nand a b o)
       (bwrite! o (not (and (bread a) (bread b))))]
      [(clock ib os)
       (define ni (add1 (unbox ib)))
       (set-box! ib ni)
       (for ([o (in-list os)]
             [i (in-naturals)])
         (bwrite! o (= i ni)))]
      [(latch ? pb nb)
       (when (bread ?)
         (set-box! pb (unbox nb)))]))

  (tree-walk sn ustep))

;; Exhaustive testing
(module+ test
  (define (chk-tt f ls)
    (with-chk (['f f])
      (for ([l (in-list ls)])
        (match-define (list ins outs) l)
        (define inws (Bundle (length ins)))
        (define outws (Bundle (length outs)))
        (define n (apply f (append inws outws)))
        (for ([in (in-list ins)]
              [inw (in-list inws)])
          (bwrite! inw (= 1 in)))
        (simulate! n)
        (with-chk (['ins ins])
          (chk (map (λ (w) (if (bread w) 1 0)) outws)
               outs))))))

;; Functional Units
(module+ test
  (chk-tt Nand
          '(((0 0) (1))
            ((0 1) (1))
            ((1 0) (1))
            ((1 1) (0)))))

;; xxx test clock
;; xxx test latch
;; xxx test cell

(define (Not a o)
  (Nand a a o))
(module+ test
  (chk-tt Not
          '(((0) (1))
            ((1) (0)))))

(define (Id a o)
  (Net (t)
       (Not a t)
       (Not t o)))
(module+ test
  (chk-tt Id
          '(((0) (0))
            ((1) (1)))))

(define (And a b o)
  (Net (t)
       (Nand a b t)
       (Not t o)))
(module+ test
  (chk-tt And
          '(((0 0) (0))
            ((0 1) (0))
            ((1 0) (0))
            ((1 1) (1)))))

(define (Or a b o)
  (Net (na nb)
       (Not a na)
       (Not b nb)
       (Nand na nb o)))
(module+ test
  (chk-tt Or
          '(((0 0) (0))
            ((0 1) (1))
            ((1 0) (1))
            ((1 1) (1)))))

(define (Nor a b o)
  (Net (t)
       (Or a b t)
       (Not t o)))
(module+ test
  (chk-tt Nor
          '(((0 0) (1))
            ((0 1) (0))
            ((1 0) (0))
            ((1 1) (0)))))

(define (Xor a b o)
  (Net (t at tb)
       (Nand a b t)
       (Nand a t at)
       (Nand t b tb)
       (Nand at tb o)))
(module+ test
  (chk-tt Xor
          '(((0 0) (0))
            ((0 1) (1))
            ((1 0) (1))
            ((1 1) (0)))))

(define (Xnor a b o)
  (Net (t)
       (Xor a b t)
       (Not t o)))
(module+ test
  (chk-tt Xnor
          '(((0 0) (1))
            ((0 1) (0))
            ((1 0) (0))
            ((1 1) (1)))))

;; o = (if s b a)
(define (Mux a b s o)
  (Net (as bs ns)
       (Not s ns)
       (And a ns as)
       (And b s bs)
       (Or as bs o)))
(module+ test
  (chk-tt Mux
          '(((0   0   0) (0))
            ((0   1   0) (0))
            ((1   0   0) (1))
            ((1   1   0) (1))
            ((0   0   1) (0))
            ((0   1   1) (1))
            ((1   0   1) (0))
            ((1   1   1) (1)))))

(define (Demux i s a b)
  (Net (ns)
       (Not s ns)
       (And i ns a)
       (And s i b)))
(module+ test
  (chk-tt Demux
          (for*/list ([i (in-range 2)] [s (in-range 2)])
            (list (list i s)
                  (if (= s 0)
                    (list i 0)
                    (list 0 i))))))

(define (Full-Adder a b cin cout sum)
  (Net (axb ab ct)
       (Xor a b axb)
       (Xor cin axb sum)
       (And a b ab)
       (And axb cin ct)
       (Or ab ct cout)))
(module+ test
  (chk-tt
   Full-Adder
   '(((0 0 0) (0 0))
     ((0 0 1) (0 1))
     ((0 1 0) (0 1))
     ((0 1 1) (1 0))
     ((1 0 0) (0 1))
     ((1 0 1) (1 0))
     ((1 1 0) (1 0))
     ((1 1 1) (1 1)))))

(module+ test
  (define-syntax (define-chk-num stx)
    (syntax-parse stx
      [(_ the-chk:id
          #:N N:id
          #:in (iw:wire-spec ...)
          #:out (ow:wire-spec ...)
          #:circuit the-circuit:id
          #:exhaust MAX-N:expr
          #:check check-e:expr)
       (syntax/loc stx
         (begin
           (define (the-chk #:N N iw.i ...)
             (with-chk (['Circuit 'the-circuit]
                        ['iw.i iw.i] ...)
               (define-wires ow ...)
               (define (in-write v w)
                 (if (list? w)
                   (write-number! w v)
                   (bwrite! w v))
                 w)
               (define some-net (the-circuit (in-write iw.i iw.d) ... ow.i ...))
               (simulate! some-net)
               (define (out-raw w)
                 (if (list? w) (map bread w) (bread w)))
               (define (out-read w)
                 (if (list? w) (read-number w) (bread w)))
               (with-chk (['ow.i (out-raw ow.i)] ...)
                 (let ([ow.i (out-read ow.i)] ...)
                   (with-chk ([(string->symbol (format "read:~a" 'ow.i))
                               ow.i] ...)
                     check-e)))))

           (for ([N (in-range 1 MAX-N)])
             (define MAX-V (expt 2 N))
             (define (in-iter w)
               (if (list? w) (in-range MAX-V) (in-list '(#f #t))))
             (for* ([iw.i (in-iter iw.d)] ...)
               (the-chk #:N N iw.i ...)))))])))

(define (Adder/N A B Cin Cout Sum)
  (define N (length A))
  (when (zero? N) (error 'Adder/N "Cannot add to 0 bits"))
  (unless (= N (length B) (length Sum))
    (error 'Adder/N "sizes mismatch"))
  (define Cs (Bundle (sub1 N)))
  (define Cins (cons Cin Cs))
  (define Couts (snoc Cs Cout))
  (map Full-Adder A B Cins Couts Sum))
(module+ test
  (define-chk-num chk-adder
    #:N N #:in ([A N] [B N] Cin) #:out (Cout [Sum N])
    #:circuit Adder/N #:exhaust 5
    #:check
    (chk (cons Sum Cout)
         (cons (modulo (+ A B (if Cin 1 0)) (expt 2 N))
               (> (+ A B (if Cin 1 0)) (sub1 (expt 2 N))))))

  (chk-adder #:N 16 4011 777 #f))

(define (Dupe/N src dst)
  (map Id src dst))
(module+ test
  (define-chk-num chk-dupe
    #:N N #:in ([Src N]) #:out ([Dst N])
    #:circuit Dupe/N #:exhaust 5
    #:check (chk Dst Src)))

(define (Half-Adder A B C S)
  (Net ()
       (Xor A B S)
       (And A B C)))
(module+ test
  (chk-tt
   Half-Adder
   '(((0    0)  (0  0))
     ((1    0)  (0  1))
     ((0    1)  (0  1))
     ((1    1)  (1  0)))))

(define (Increment/N A Cout Inc)
  (define N (length A))
  (when (zero? N)
    (error 'Increment/N "Cannot increment 0 bits"))
  (unless (= N (length Inc))
    (error 'Increment/N "Output is not same length as input"))
  (define-wires [Cs (sub1 N)])
  (define B (cons TRUE Cs))
  (define C (snoc Cs Cout))
  (map Half-Adder A B C Inc))
(module+ test
  (define-chk-num chk-increment
    #:N N #:in ([A N]) #:out (Cout [Inc N])
    #:circuit Increment/N #:exhaust 5
    #:check
    (chk (cons Inc Cout)
         (cons (modulo (+ A 1) (expt 2 N))
               (> (+ A 1) (sub1 (expt 2 N)))))))

(define (Shifter Left? Right? L Z R O)
  (Net (Z-or-L)
       (Mux Z L Left? Z-or-L)
       (Mux Z-or-L R Right? O)))
(module+ test
  (chk-tt Shifter
          (for*/list ([L? (in-range 2)] [R? (in-range 2)]
                      [L (in-range 2)] [Z (in-range 2)] [R (in-range 2)])
            (list (list L? R? L Z R)
                  (list (cond [(= 1 R?) R]
                              [(= 1 L?) L]
                              [else Z]))))))

(define (Shifter/N Left? Right? In Out)
  (define N (length In))
  (unless (= N (length Out))
    (error 'Shifter/N "In/Out do not match in length"))
  (define (tser l) (reverse (rest (reverse l))))
  (define Lefts (cons FALSE (tser In)))
  (define Rights (snoc (rest In) FALSE))
  (map (λ (l i r o) (Shifter Left? Right? l i r o))
       Lefts In Rights Out))
(module+ test
  (define-chk-num chk-shifter
    #:N N #:in (Left? Right? [In N]) #:out ([Out N])
    #:circuit Shifter/N #:exhaust 5
    #:check
    (chk Out
         (modulo (arithmetic-shift In
                                   (cond [Right? -1]
                                         [Left? +1] 
                                         [else 0]))
                 (expt 2 N)))))

;; XXX
(define Decoder void)
(define Encoder void)
(define ControlStore void)
(define RegisterDecoder void)
(define Latch void)
(define ALU void)
(define MicroSeqLogic void)
(define RegisterSet void)

(define (MIC-1 Microcode
               Registers
               Read? Write?
               MAR MAR?
               MBR MBR?)
  (define μAddrSpace (integer-length (vector-length Microcode)))
  (define WordBits (length MBR))
  (define RegisterCount (length Registers))
  (define RegisterBits (integer-length RegisterCount))

  ;; Aliases
  (define B-latch-out MAR)
  (define MIR:RD Read?)
  (define MIR:WR Write?)
  (define C-Bus MBR)
  (define Shifter-out C-Bus)
  (Net (GROUND
        Clock:1 Clock:2 Clock:3 Clock:4
        N Z MicroSeqLogic-out
        MIR:AMUX [MIR:COND 2] [MIR:ALU 2] [MIR:SH 2]
        MIR:MBR MIR:MAR MIR:RD MIR:WR
        MIR:ENC
        [MIR:C RegisterBits] [MIR:B RegisterBits] [MIR:A RegisterBits]
        [MIR:ADDR μAddrSpace]
        [MPC-out μAddrSpace] [Mmux-out μAddrSpace]
        MPC-Inc-carry [MPC-Inc-out μAddrSpace]
        [Asel RegisterCount] [Bsel RegisterCount] [Csel RegisterCount]

        [A-Bus WordBits] [B-Bus WordBits] [C-Bus WordBits]
        [A-latch-out WordBits]
        [Amux-out WordBits] [ALU-out WordBits]
        Shifter-Left? Shifter-Right? [Shifter-out WordBits])

       (Clock Clock:1 Clock:2 Clock:3 Clock:4)
       (ControlStore Microcode
                     Clock:1 MPC-out
                     MIR:AMUX MIR:COND MIR:ALU MIR:SH
                     MIR:MBR MIR:MAR MIR:RD MIR:WR
                     MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR)
       (RegisterDecoder TRUE MIR:B TRUE Bsel)
       (RegisterDecoder TRUE MIR:A TRUE Asel)
       (Latch Clock:2 A-Bus A-latch-out)
       (Latch Clock:2 B-Bus B-latch-out)
       (Mux MIR:AMUX MBR A-latch-out Amux-out)
       (ALU Amux-out B-Bus MIR:ALU ALU-out N Z)
       (MicroSeqLogic N Z MIR:COND MicroSeqLogic-out)
       (Mux MicroSeqLogic-out MPC-Inc-out MIR:ADDR Mmux-out)
       (Decoder MIR:SH (list GROUND Shifter-Right? Shifter-Left? GROUND))
       (Shifter/N Shifter-Left? Shifter-Right? ALU-out Shifter-out)
       (RegisterDecoder Clock:4 MIR:C MIR:ENC Csel)
       (RegisterSet Registers Clock:4 C-Bus Csel Asel Bsel A-Bus B-Bus)
       (Latch Clock:4 Mmux-out MPC-out)
       (Increment/N MPC-out MPC-Inc-carry MPC-Inc-out)

       (And MIR:MAR Clock:3 MAR?)
       (And MIR:MBR Clock:4 MBR?)))

(module+ main
  (define Microcode
    ;; XXX
    (vector 0))
  (define-wires
    Read? Write?
    [MAR 16] MAR?
    [MBR 16] MBR?)
  (define Registers
    (build-list 16 (λ (i) (Bundle 16))))
  (define the-mic1
    (MIC-1 Microcode
           Registers
           Read? Write?
           MAR MAR?
           MBR MBR?))
  (format "MIC-1 has ~a NAND gates"
          (net-count the-mic1))
  ;; XXX do something with the-mic1

  )
