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
(struct wire (debug value)
  #:methods gen:custom-write
  [(define (write-proc w p m)
     (fprintf p "#<wire: ~a>" (wire-debug w)))])
;; xxx remove and use SR-latch?
(struct latch (latch? prev next))
(define (wirelike? w)
  (or (wire? w) (latch? w)))

(struct nand (a b o))

;; Constructors
(define Mt null)
(begin-for-syntax
  (define-syntax-class wire-spec
    #:attributes (i d)
    (pattern i:id
             #:attr d (syntax/loc #'i (Wire #:debug 'i)))
    (pattern [i:id n:expr]
             #:attr d (syntax/loc #'i (Bundle #:debug 'i n)))))
(define-simple-macro (define-wires w:wire-spec ...)
  (begin (define w.i w.d) ...))
(define-simple-macro (Net (w:wire-spec ...) b ...)
  (let ()
    (define-wires w ...)
    (list b ...)))
(define (Cell) (latch TRUE (box #f) (box #f)))
(define (Cells n) (build-list n (λ (i) (Cell))))

(define (Wire #:debug [d (gensym)])
  (wire d (box #f)))
(define (Bundle #:debug [d (gensym)] n)
  (build-list n (λ (i) (Wire #:debug (cons d i)))))

(define TRUE (wire 'TRUE (box-immutable #t)))
(define FALSE (wire 'FALSE (box-immutable #f)))
(define GROUND (Wire #:debug 'GROUND))

(define (Nand a b o)
  (unless (wirelike? a) (error 'Nand "Expected wire for a, got: ~v" a))
  (unless (wirelike? b) (error 'Nand "Expected wire for b, got: ~v" b))
  (unless (wirelike? o) (error 'Nand "Expected wire for o, got: ~v" o))
  (when (or (eq? o TRUE) (eq? o FALSE)) (error 'Nand "Cannot write to constants"))
  (when (or (eq? a GROUND) (eq? b GROUND)) (error 'Nand "Cannot read ground"))
  (nand a b o))

;; Simulator
(define bread
  (match-lambda
    [(wire _ vb) (unbox vb)]
    [(latch _ pb n) (unbox pb)]))
(define (bwrite! b ?)
  (match b
    [(wire _ vb)
     (set-box! vb ?)]
    [(latch latch? _ nb)
     (when (bread latch?)
       (set-box! nb ?))]))

(define (tree-walk n f)
  (match n
    [(cons a d)
     (tree-walk a f)
     (tree-walk d f)]
    [(or #f '() (? void?))
     (void)]
    [x
     (f x)]))

(define (simulate! sn)
  (tree-walk
   sn
   (match-lambda
     [(nand a b o)
      (bwrite! o (not (and (bread a) (bread b))))]
     [(latch ? pb nb)
      (when (bread ?)
        (set-box! pb (unbox nb)))])))

;; xxx compile to C (or LLVM assembly)

;; Helpers
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

;; xxx check that every wire is only set once, other than Ground?
;; xxx general optimizations?
(define (net-count sn)
  (define C 0)
  (tree-walk sn (λ (_) (set! C (add1 C))))
  C)

;; Exhaustive testing
(module+ test
  (define (tt-make set? in)
    (cond
      [(list? in)
       (for/list ([i (in-list in)])
         (tt-make set? i))]
      [else
       (define w (Wire))
       (when set? (bwrite! w (= 1 in)))
       w]))
  (define (tt-check-out outw out)
    (cond
      [(list? outw) (for-each tt-check-out outw out)]
      [else
       (chk (if (bread outw) 1 0) out)]))
  (define (chk-tt f ls)
    (with-chk (['f f])
      (for ([l (in-list ls)])
        (match-define (list ins outs) l)
        (define inws (tt-make #t ins))
        (define outws (tt-make #f outs))
        (define n (apply f (append inws outws)))
        (simulate! n)
        (with-chk (['ins ins])
          (tt-check-out outws outs))))))
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
             (with-chk (['N N]
                        ['Circuit 'the-circuit]
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

;; Functional Units
(module+ test
  (chk-tt Nand
          '(((0 0) (1))
            ((0 1) (1))
            ((1 0) (1))
            ((1 1) (0)))))

;; xxx test latch
;; xxx test cell

(define (Not a o)
  (Nand a a o))
(module+ test
  (chk-tt Not
          '(((0) (1))
            ((1) (0)))))

(define (Not/N A O)
  (map Not A O))
(module+ test
  (define-chk-num chk-not
    #:N N #:in ([A N]) #:out ([O N])
    #:circuit Not/N #:exhaust 5
    #:check
    (chk O (modulo (bitwise-not A) (expt 2 N)))))

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

(define (binary->nary Op Unit)
  (define (nary l)
    (match l
      [(list Out) (Id Unit Out)]
      [(list X Out) (Id X Out)]
      [(list A B Out) (Op A B Out)]
      [(list* A B More)
       (Net (T)
            (Op A B T)
            (nary (cons T More)))]))
  (λ l (nary l)))

(define And* (binary->nary And TRUE))
(module+ test
  (chk-tt And*
          '(((0 0 0) (0))
            ((0 0 1) (0))
            ((0 1 0) (0))
            ((0 1 1) (0))
            ((1 0 0) (0))
            ((1 0 1) (0))
            ((1 1 0) (0))
            ((1 1 1) (1)))))

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

(define Or* (binary->nary Or FALSE))
(module+ test
  (chk-tt Or*
          '(((0 0 0) (0))
            ((0 0 1) (1))
            ((0 1 0) (1))
            ((0 1 1) (1))
            ((1 0 0) (1))
            ((1 0 1) (1))
            ((1 1 0) (1))
            ((1 1 1) (1)))))

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

(define (Mux/N A B s O)
  (map (λ (a b o) (Mux a b s o))
       A B O))
(module+ test
  (chk-tt Mux/N
          '((((0) (1) 0) ((0)))
            (((0) (1) 1) ((1)))

            (((0 0) (1 1) 0) ((0 0)))
            (((0 0) (1 1) 1) ((1 1))))))

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

(define (Adder/N A B Cin Cout Sum)
  (define N (length A))
  (when (zero? N) (error 'Adder/N "Cannot add to 0 bits"))
  (unless (= N (length B) (length Sum))
    (error 'Adder/N "sizes mismatch"))
  (define-wires [Cs (sub1 N)])
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

(define (Decoder A*B A B)
  (Net ()
       (Not A*B A)
       (Id A*B B)))
(module+ test
  (chk-tt
   Decoder
   '(((0) (1 0))
     ((1) (0 1)))))

(define (And/wb w0 bi bo)
  (map (λ (w1 w2) (And w0 w1 w2)) bi bo))

(define (Decoder/N Which Outs)
  (define N (length Which))
  (unless (= (length Outs) (expt 2 N))
    (error 'Decoder/N "Insufficient output signals: N=~v, ~v should be ~v"
           N (length Outs) (expt 2 N)))
  (let loop ([N N] [Which (reverse Which)] [Outs Outs])
    (cond
      [(= N 1) (Decoder (first Which) (first Outs) (second Outs))]
      [else
       ;; NOTE It might be possible to do this more efficient with the
       ;; recursion removing half of the bits to decode each time, but
       ;; I'm not sure how to generalize it.
       (define next-N (sub1 N))
       (define next-2N (expt 2 next-N))
       (define-values (fst-Outs snd-Outs) (split-at Outs next-2N))
       (Net (OnTop OnBottom [NewOuts next-2N])
            (Decoder (first Which) OnBottom OnTop)
            (loop next-N (rest Which) NewOuts)
            (And/wb OnBottom NewOuts fst-Outs)
            (And/wb OnTop NewOuts snd-Outs))])))
(module+ test
  (chk-tt
   Decoder/N
   '([((0)) ((1 0))]
     [((1)) ((0 1))]))
  
  (define-chk-num chk-decoder
    #:N N #:in ([Which N]) #:out ([Outs (expt 2 N)])
    #:circuit Decoder/N #:exhaust 6
    #:check
    (chk Outs (arithmetic-shift 1 Which)))

  (chk-decoder #:N 3 #b111)
  (chk (arithmetic-shift 1 7) 128))

(define (log2 x)
  (define r (/ (log x) (log 2)))
  (and (integer? r)
       (inexact->exact r)))
(module+ test
  (for ([i (in-range 1 10)])
    (define x (expt 2 i))
    (chk (log2 x) i
         (log2 (add1 x)) #f)))
(define (Clock Os)
  (define 2N (length Os))
  (define N (log2 2N))
  (unless (and N (> N 0))
    (error 'Clock "Must receive 2^n, n>0 output signals"))
  (define Code (Cells N))
  (Net ()
       Code
       (Increment/N Code GROUND Code)
       (Decoder/N Code Os)))
(module+ test
  (define (chk-clock N)
    (with-chk (['N N])
      (define-wires [Os N])
      (define C (Clock Os))
      (for ([i (in-range N)])
        (with-chk (['i i])
          (simulate! C)
          (chk (read-number Os)
               (arithmetic-shift 1 i))))))
  (for ([n (in-range 1 3)])
    (chk-clock (expt 2 n))))

(define (MicroSeqLogic N Z Code Out)
  (Net (JumpOnN! JumpOnZ! NoJump JumpOnN JumpOnZ JumpAlways)
       (Decoder/N Code (list NoJump JumpOnN JumpOnZ JumpAlways))
       (And JumpOnN N JumpOnN!)
       (And JumpOnZ Z JumpOnZ!)
       (Or* JumpOnN! JumpOnZ! JumpAlways Out)))
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

(define (ROM-AddrSpace vals)
  (integer-length (sub1 (length vals))))

(define (number->wires N n)
  (for/list ([i (in-range N)])
    (if (bitwise-bit-set? n i) TRUE FALSE)))
(module+ test
  (for* ([N (in-range 1 8)]
         [n (in-range (expt 2 N))])
    (chk (read-number (number->wires N n)) n)))

(define (numbers->wires N ns)
  (map (λ (n) (number->wires N n)) ns))

(define (ROM-1bit ValBits Which ValueBitOut)
  (unless (= (length ValBits) (length Which))
    (error 'ROM-1bit "Mismatch of signals and bits"))
  (Net ([vb*ws (length ValBits)])
       (for/list ([vb (in-list ValBits)]
                  [w (in-list Which)]
                  [vb*w (in-list vb*ws)])
         ;; If the value bit is set, and this is the value we want,
         ;; set it to the vb*w
         (And vb w vb*w))
       ;; Then or all these together and send the output to ValueBitOut
       (apply Or* (snoc vb*ws ValueBitOut))))
(module+ test
  (chk-tt ROM-1bit
          '([((0 1) (0 0)) (0)]
            [((0 1) (1 0)) (0)]
            [((0 1) (0 1)) (1)]

            [((1 0) (0 0)) (0)]
            [((1 0) (1 0)) (1)]
            [((1 0) (0 1)) (0)]

            [((0 0) (0 0)) (0)]
            [((0 0) (1 0)) (0)]
            [((0 0) (0 1)) (0)]

            [((1 1) (0 0)) (0)]
            [((1 1) (1 0)) (1)]
            [((1 1) (0 1)) (1)])))

(define (ROM vals Addr Value)
  (define A (ROM-AddrSpace vals))
  (define W (length Value))
  (unless (= A (length Addr))
    (error 'ROM "Addr wrong bits: got ~v, expected ~v" (length Addr) A))

  ;; The ROM is 2^A different values in the `vals` list. First, we
  ;; decode the address into a signal that says "Get value i".
  (define-wires [Which (expt 2 A)])
  (define decode-net (Decoder/N Addr Which))

  ;; Each value in the ROM is W bits long
  (define val-wires (numbers->wires W vals))

  ;; So, we have a different circuit for each bit of the ROM's output
  (define set-value-net
    (for/list ([Value_i (in-list Value)]
               [i (in-naturals)])
      (ROM-1bit (map (λ (vw) (list-ref vw i)) val-wires)
                Which
                Value_i)))

  ;; We return the composition of these
  (Net () decode-net set-value-net))
(module+ test
  (define test-2rom-vals '(0 1))
  (chk (ROM-AddrSpace test-2rom-vals) 1)
  (define (test-2rom Addr Value)
    (ROM test-2rom-vals Addr Value))
  (chk-tt test-2rom
          '([((0)) ((0))]
            [((1)) ((1))]))
  
  (define (chk-rom N vals)
    (define-wires
      [Addr (ROM-AddrSpace vals)]
      [Value N])
    (define n (ROM vals Addr Value))
    (for ([i (in-naturals)]
          [v (in-list vals)])
      (write-number! Addr i)
      (simulate! n)
      (with-chk (['i i]
                 ['v v])
        (chk (read-number Value) v))))

  (for ([N (in-range 1 2)])
    (with-chk (['N N])
      (chk-rom
       N
       (for/list ([i (in-range (expt 2 N))])
         i)))))

(define (Cut/N Src Dst)
  (define FS (flatten Src))
  (define SrcN (length FS))
  (define FD (flatten Dst))
  (define DstN (length FD))
  (unless (= SrcN DstN)
    (error 'Cut/N "Source(~v) and dest(~v) are not same length"
           SrcN DstN))
  (Dupe/N FS FD))
(module+ test
  (chk-tt Cut/N
          '([(( 0 1  0  1 1 0  0 1))
             (((0 1) 0 (1 1 0) 0 1))])))

;; XXX
(define Latch void)
(define ALU void)
(define RegisterRead void)
(define RegisterSet void)

(define (MIC-1 μCodeLength Microcode
               Registers
               Read? Write?
               MAR MAR?
               MBR MBR?)
  (define μAddrSpace (ROM-AddrSpace Microcode))
  (define WordBits (length MBR))
  (define RegisterCount (length Registers))
  (define RegisterBits (integer-length (sub1 RegisterCount)))

  ;; Aliases
  (define B-latch-out MAR)
  (define MIR:RD Read?)
  (define MIR:WR Write?)
  (define C-Bus MBR)
  (define Shifter-out C-Bus)
  (define-wires
    Clock:1 Clock:2 Clock:3 Clock:4
    N Z MicroSeqLogic-out
    [pre-MIR μCodeLength] [MIR μCodeLength]
    MIR:AMUX [MIR:COND 2] [MIR:ALU 2] [MIR:SH 2]
    MIR:MBR MIR:MAR
    MIR:ENC
    [MIR:C RegisterBits] [MIR:B RegisterBits] [MIR:A RegisterBits]
    [MIR:ADDR μAddrSpace]
    [MPC-out μAddrSpace] [Mmux-out μAddrSpace]
    MPC-Inc-carry [MPC-Inc-out μAddrSpace]
    [Asel RegisterCount] [Bsel RegisterCount] [Csel RegisterCount]

    [A-Bus WordBits] [B-Bus WordBits]
    [A-latch-out WordBits]
    [Amux-out WordBits] [ALU-out WordBits]
    Shifter-Left? Shifter-Right? Write-C?)
  (Net ()
       (Clock (list Clock:1 Clock:2 Clock:3 Clock:4))
       (ROM Microcode MPC-out pre-MIR)
       (Latch Clock:1 pre-MIR MIR)
       (Cut/N MIR
              (list MIR:AMUX MIR:COND MIR:ALU MIR:SH
                    MIR:MBR MIR:MAR MIR:RD MIR:WR
                    MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR))
       (Decoder/N MIR:A Asel)
       (Decoder/N MIR:B Bsel)
       (Decoder/N MIR:C Csel)
       (RegisterRead Registers Asel A-Bus)
       (RegisterRead Registers Bsel B-Bus)
       (Latch Clock:2 A-Bus A-latch-out)
       (Latch Clock:2 B-Bus B-latch-out)
       (Mux/N MBR A-latch-out MIR:AMUX Amux-out)
       (ALU Amux-out B-Bus MIR:ALU ALU-out N Z)
       (MicroSeqLogic N Z MIR:COND MicroSeqLogic-out)
       (Mux/N MPC-Inc-out MIR:ADDR MicroSeqLogic-out Mmux-out)
       (Decoder/N MIR:SH (list GROUND Shifter-Right? Shifter-Left? GROUND))
       (Shifter/N Shifter-Left? Shifter-Right? ALU-out Shifter-out)
       (And Clock:4 MIR:ENC Write-C?)
       (RegisterSet Registers Write-C? C-Bus Csel)
       (Latch Clock:4 Mmux-out MPC-out)
       (Increment/N MPC-out MPC-Inc-carry MPC-Inc-out)

       (And MIR:MAR Clock:3 MAR?)
       (And MIR:MBR Clock:4 MBR?)))

(module+ main
  (define WordSize 16)
  (define RegisterCount 16)
  (define Microcode (make-list 256 0))
  (define-wires
    Read? Write?
    [MAR WordSize] MAR?
    [MBR WordSize] MBR?)
  (define Registers
    (build-list RegisterCount (λ (i) (Cells WordSize))))
  (define the-mic1
    (MIC-1 32 Microcode
           Registers
           Read? Write?
           MAR MAR?
           MBR MBR?))
  (format "MIC-1 has ~a NAND gates"
          (net-count the-mic1))
  ;; XXX do something with the-mic1

  )
