#lang racket/base
(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require chk))

;; Lib
(define (snoc l x) (append l (list x)))

;; Core
(struct wire (value))

(struct Nand (a b o))
;; xxx remove
(struct latch (latch? prev next))
;; xxx remove
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

(define (simulate! sn)
  (define (tree-walk n f)
    (match n
      [(cons a d)
       (+ (tree-walk a f)
          (tree-walk d f))]
      [(or #f '() (? void?))
       0]
      [x
       (f x)
       1]))

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

(define (Mux a b s o)
  (Net (as bs ns)
       (Not s ns)
       (And a ns as)
       (And b s bs)
       (Or as bs o)))
;; XXX

(define (Demux i s a b)
  (Net (ns na nb)
       (Not s ns)
       (Nand i ns na)
       (Not na a)
       (Nand s i)
       (Not nb b)))
;; XXX

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

(define (Double-Adder A0 A1 B0 B1 Cin Cout Sum0 Sum1)
  (Net (C0)
       (Full-Adder A0 B0 Cin C0 Sum0)
       (Full-Adder A1 B1 C0 Cout Sum1)))
(module+ test
  (chk-tt
   Double-Adder
   '(((0 0 0 0 0) (0 0 0)) ;; 0 + 0 = 0

     ((1 0 0 0 0) (0 1 0)) ;; 1 + 0 = 1
     ((0 0 1 0 0) (0 1 0)) ;; 0 + 1 = 1
     ((1 0 1 0 0) (0 0 1)) ;; 1 + 1 = 2

     ((0 1 0 0 0) (0 0 1)) ;; 2 + 0 = 2
     ((0 0 0 1 0) (0 0 1)) ;; 0 + 2 = 2
     ((0 1 1 0 0) (0 1 1)) ;; 2 + 1 = 3
     ((1 0 0 1 0) (0 1 1)) ;; 1 + 2 = 3
     ((0 1 0 1 0) (1 0 0)) ;; 2 + 2 = 4

     ((1 1 0 0 0) (0 1 1)) ;; 3 + 0 = 3
     ((1 1 1 0 0) (1 0 0)) ;; 3 + 1 = 4
     ((1 1 0 1 0) (1 1 0)) ;; 3 + 2 = 5
     ((1 1 1 1 0) (1 0 1)) ;; 3 + 3 = 6
     ((0 0 1 1 0) (0 1 1)) ;; 0 + 3 = 3
     ((1 0 1 1 0) (1 0 0)) ;; 1 + 3 = 4
     ((0 1 1 1 0) (1 1 0)) ;; 2 + 3 = 5
     )))

(define (Adder A B Cin Cout Sum)
  (define N (length A))
  (unless (and (= N (length B))
               (= N (length Sum)))
    (error 'Adder "A ≠ B ≠ Sum"))
  (define Cs (Bundle (sub1 N)))
  (define Cins (cons Cin Cs))
  (define Couts (snoc Cs Cout))
  (for/fold ([prev-Net Mt])
            ([a (in-list A)]
             [b (in-list B)]
             [cin (in-list Cins)]
             [cout (in-list Couts)]
             [s (in-list Sum)]
             [i (in-naturals)])
    (Net ()
         prev-Net
         (Full-Adder a b cin cout s))))
(module+ test
  (define (chk-adder a b #:N [GN #f])
    (define N (or GN (max (integer-length a) (integer-length b))))
    (with-chk (['a a]
               ['b b]
               ['N N])
      (define-wires [A N] [B N] Cin [Sum N] Cout)
      (write-number! A a)
      (write-number! B b)
      (define some-net (Adder A B Cin Cout Sum))
      (simulate! some-net)
      (with-chk (['Sum (map bread Sum)])
        (chk (cons (read-number Sum)
                   (bread Cout))
             (cons (modulo (+ a b) (expt 2 N))
                   (> (+ a b) (sub1 (expt 2 N))))))))


  (for ([N (in-range 5)])
    (define MAX (expt 2 N))
    (for* ([a (in-range MAX)] [b (in-range MAX)])
      (chk-adder a b #:N N)))

  (chk-adder 4011 777))

(define (Dupe src dst)
  (cond
    [(and (wire? src) (wire? dst))
     (Id src dst)]
    [else
     (map Dupe src dst)]))

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

(define (Increment A Cout Inc)
  (define N (length A))
  (unless (= N (length Inc))
    (error 'Increment "Output is not same length as input"))
  (define-wires [Cs (sub1 N)])
  (define B (cons TRUE Cs))
  (define C (snoc Cs Cout))
  (for/fold ([nt Mt])
            ([a (in-list A)]
             [b (in-list B)]
             [c (in-list C)]
             [i (in-list Inc)])
    (Net () nt (Half-Adder a b c i))))
(module+ test
  (define (chk-increment a #:N [GN #f])
    (define N (or GN (integer-length a)))
    (with-chk (['a a]
               ['N N])
      (define-wires [A N] [Inc N] Cout)
      (write-number! A a)
      (define some-net (Increment A Cout Inc))
      (simulate! some-net)
      (with-chk (['Inc (map bread Inc)])
        (chk (cons (read-number Inc)
                   (bread Cout))
             (cons (modulo (+ a 1) (expt 2 N))
                   (> (+ a 1) (sub1 (expt 2 N))))))))

  (for ([N (in-range 1 5)])
    (define MAX (expt 2 N))
    (for* ([a (in-range MAX)])
      (chk-increment a #:N N))))

;; XXX
(define ControlStore void)
(define RegisterDecoder void)
(define Latch void)
(define ALU void)
(define MicroSeqLogic void)
(define Shifter void)
(define RegisterSet void)

;; XXX
;; XXX registers need to be arguments so the debugger can read them
(define (MIC-1 Microcode
               Read? Write?
               MAR MAR?
               MBR MBR?)
  (define μAddrSpace 8)
  (define WordBits 16)
  (define RegisterCount 16)
  (define RegisterBits (integer-length RegisterCount))

  ;; Aliases
  (define B-latch-out MAR)
  (define MIR:RD Read?)
  (define MIR:WR Write?)
  (define C-Bus MBR)
  (define Shifter-out C-Bus)
  (Net (#;_
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
        [Amux-out WordBits] [ALU-out WordBits] [Shifter-out WordBits])

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
       (Shifter ALU-out MIR:SH Shifter-out)
       (RegisterDecoder Clock:4 MIR:C MIR:ENC Csel)
       (RegisterSet Clock:4 C-Bus Csel Asel Bsel A-Bus B-Bus)
       (Latch Clock:4 Mmux-out MPC-out)
       (Increment MPC-out MPC-Inc-carry MPC-Inc-out)

       (And MIR:MAR Clock:3 MAR?)
       (And MIR:MBR Clock:4 MBR?)))
(module+ main
  (define Microcode
    ;; XXX
    (vector))
  (define-wires
    Read? Write?
    [MAR 16] MAR?
    [MBR 16] MBR?)
  ;; XXX
  (MIC-1 Microcode
         Read? Write?
         MAR MAR?
         MBR MBR?))
