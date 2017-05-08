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

(struct nand (a b o))
(define HDL-DEBUG? (make-parameter #f))
(struct debug (f))

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

(define (Wire #:debug [d (gensym)])
  (wire d (box #f)))
(define (Bundle #:debug [d (gensym)] n)
  (build-list n (λ (i) (Wire #:debug (cons d i)))))

(define TRUE (wire 'TRUE (box-immutable #t)))
(define FALSE (wire 'FALSE (box-immutable #f)))
(define GROUND (Wire #:debug 'GROUND))

(define (Nand a b o)
  (unless (wire? a) (error 'Nand "Expected wire for a, got: ~v" a))
  (unless (wire? b) (error 'Nand "Expected wire for b, got: ~v" b))
  (unless (wire? o) (error 'Nand "Expected wire for o, got: ~v" o))
  (when (or (eq? o TRUE) (eq? o FALSE)) (error 'Nand "Cannot write to constants"))
  (when (or (eq? a GROUND) (eq? b GROUND)) (error 'Nand "Cannot read ground"))
  (nand a b o))

;; Simulator
(define (bread b)
  (match-define (wire _ vb) b)
  (unbox vb))
(define (bwrite! b ?)
  (match-define (wire _ vb) b)
  (set-box! vb ?))

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
     [(debug f) (when (HDL-DEBUG?) (f))]
     [(nand a b o)
      (bwrite! o (not (and (bread a) (bread b))))])))

;; Helpers
(define (bwriten! b n)
  (bwrite! b (= n 1)))
(define (breadn b)
  (if (bread b) 1 0))

(define (write-number! B n)
  (cond
    [(list? B)
     (define len (integer-length n))
     (define Blen (length B))
     (unless (<= len Blen)
       (error 'write-number! "Not enough bits(~v) for number(~v [~v bits]):"
              Blen n len))
     (for ([b (in-list B)]
           [i (in-naturals)])
       (bwrite! b (bitwise-bit-set? n i)))]
    [else
     (bwriten! B n)]))
(define (read-number B)
  (cond
    [(list? B)
     (for/fold ([n 0]) ([b (in-list B)] [i (in-naturals)])
       (+ n (* (breadn b) (expt 2 i))))]
    [else
     (breadn B)]))

;; Analysis

;; xxx perform optimzations on the network? remove not not? remove
;; duplication? represent an output wire as the nand and hash-cons
;; those? iterate to fix-point?

(define (analyze sn
                 #:compile? [compile? #f]
                 #:label [label "Circuit"])
  (define Gates 0)
  (define WireUses (make-hasheq))
  (define WireSets (make-hasheq))
  (tree-walk
   sn
   (match-lambda
     [(debug f) (void)]
     [(nand a b o)
      (set! Gates (add1 Gates))
      (for ([x (in-list (list a b o))])
        (hash-update! WireUses x add1 0))
      (hash-update! WireSets o add1 0)]))

  (for ([(w c) (in-hash WireSets)]
        #:unless (eq? w GROUND)
        #:when (> c 1))
    (eprintf "~a wire set more than once.\n" w))

  (eprintf "~a has ~a NAND gates and ~a wires\n"
           label Gates (hash-count WireUses))

  ;; xxx move compiler to another file
  (when compile?
    (with-output-to-file (path-add-extension label #".c")
      #:exists 'replace
      (λ ()
        (define Wire->Id (make-hasheq))

        (printf "// Wires\n")
        (for ([w (in-hash-keys WireUses)])
          (define id (gensym 'w))
          (hash-set! Wire->Id w id)
          (printf "char ~a = 0;\n" id))
        (printf "\n")

        (printf "void cycle() {\n")
        (tree-walk
         sn
         (match-lambda
           [(debug f) (void)]
           [(nand a b o)
            (printf "\t~a = !(~a & ~a);\n"
                    (hash-ref Wire->Id o)
                    (hash-ref Wire->Id a)
                    (hash-ref Wire->Id b))]))
        (printf "}\n")))

    ;; opt -S -O3 MIC1.ll > MIC1.opt.ll
    ;; llc -O3 MIC1.opt.ll
    (with-output-to-file (path-add-extension label #".ll")
      #:exists 'replace
      (λ ()
        (define Wire->Idx (make-hasheq))
        (for ([w (in-hash-keys WireUses)]
              [i (in-naturals)])
          (hash-set! Wire->Idx w i))
        (define WC (hash-count Wire->Idx))
        ;; xxx maybe it would be more efficient to divide into word sized things
        (define WType (format "[~a x i1]" WC))

        (printf "; Wires\n")
        (printf "@WIRES = global ~a zeroinitializer, align 1\n" WType)
        (printf "\n")

        (printf "define void @cycle() {\n")

        (printf "\t; Load\n")
        (printf "\t%WIRES0 = load ~a, ~a* @WIRES, align 1\n" WType WType)
        (define Wire->Ver (make-hasheq))
        (for ([(w idx) (in-hash Wire->Idx)])
          (printf "\t%w_~a_~a = extractvalue ~a %WIRES~a, ~a\n"
                  idx 0 WType 0 idx)
          (hash-set! Wire->Ver w 0))


        (printf "\t; Work\n")
        (define cur-ver 0)
        (tree-walk
         sn
         (match-lambda
           [(debug f) (void)]
           [(nand a b o)
            (define last-ver cur-ver)
            (set! cur-ver (add1 cur-ver))
            (define next-ver cur-ver)

            (define a-idx (hash-ref Wire->Idx a))
            (define b-idx (hash-ref Wire->Idx b))
            (define o-idx (hash-ref Wire->Idx o))
            (define a-ver (hash-ref Wire->Ver a))
            (define b-ver (hash-ref Wire->Ver b))

            (printf "\t%w_~a_~an = and i1 %w_~a_~a, %w_~a_~a\n"
                    o-idx next-ver a-idx a-ver b-idx b-ver)
            (printf "\t%w_~a_~a = xor i1 %w_~a_~an, 1\n"
                    o-idx next-ver o-idx next-ver)

            (hash-set! Wire->Ver o next-ver)]))

        (printf "\t; Store\n")
        (define last-wires-ver 0)
        (for ([(w idx) (in-hash Wire->Idx)]
              [next-ver (in-naturals 1)])
          (define w-last-ver (hash-ref Wire->Ver w))
          (printf "\t%WIRES~a = insertvalue ~a %WIRES~a, i1 %w_~a_~a, ~a\n"
                  next-ver WType last-wires-ver idx w-last-ver idx)
          (set! last-wires-ver next-ver))
        (printf "\tstore ~a %WIRES~a, ~a* @WIRES, align 1\n" WType last-wires-ver WType)

        (printf "\tret void\n")
        (printf "}\n")))

    ;; xxx compile directly to assembly

    ))

;; Exhaustive testing
(module+ test
  (define (tt-make set? in)
    (cond
      [(list? in)
       (for/list ([i (in-list in)])
         (tt-make set? i))]
      [(number? in)
       (define w (Wire))
       (when set? (bwriten! w in))
       w]
      [else
       in]))
  (define (tt-check-out outw out)
    (cond
      [(list? outw) (for-each tt-check-out outw out)]
      [(number? out)
       (chk (if (bread outw) 1 0) out)]
      [else
       (void)]))
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
             (define (in-iter w)
               (if (list? w)
                 (in-range (expt 2 (length w)))
                 (in-list '(#f #t))))
             (for* ([iw.i (in-iter iw.d)] ...)
               (the-chk #:N N iw.i ...)))))])))

;; Functional Units
(module+ test
  (chk-tt Nand
          '(((0 0) (1))
            ((0 1) (1))
            ((1 0) (1))
            ((1 1) (0)))))

(define (Gated-D-Latch Clk D Q)
  ;; http://ecse.bd.psu.edu/cse271/memelem.pdf
  (define S D)
  (Net (R Top Bot NQ)
       (Not D R)
       (Nand S Clk Top)
       (Nand Clk R Bot)
       (Nand Q Bot NQ)
       (Nand Top NQ Q)))
(define Latch Gated-D-Latch)
(module+ test
  (define (simulate&chk c ws seq)
    (for ([cmd (in-list seq)]
          [i (in-naturals)])
      (with-chk (['cmd cmd]
                 ['cmd-i i])
        (match-define (list before msg after) cmd)
        (with-chk (['cmd-state 'before])
          (chk (map breadn ws) before))
        (for-each (λ (w m) (unless (eq? m '_)
                             (bwriten! w m)))
                  ws msg)
        (simulate! c)
        (with-chk (['cmd-state 'after])
          (chk (map breadn ws) after)))))

  (let ()
    (define-wires sig in out)
    (define c (Latch sig in out))
    (simulate&chk
     c (list sig in out)
     '([(0 0 0) (0 1 _) (0 1 0)]
       [(0 1 0) (1 1 _) (1 1 1)]
       [(1 1 1) (0 0 _) (0 0 1)]
       [(0 0 1) (0 1 _) (0 1 1)]
       [(0 1 1) (1 0 _) (1 0 0)]))))

(define (Latch/N signal In Out)
  (map (λ (i o) (Latch signal i o)) In Out))

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

(define (And/N A B O)
  (map And A B O))

(define (binary->nary Op Unit)
  (define (nary ins Out)
    (match ins
      [(list) (Id Unit Out)]
      [(list X) (Id X Out)]
      [(list A B) (Op A B Out)]
      [More
       (define N (length More))
       (cond
         [(odd? N)
          (Net (T)
               (nary (rest More) T)
               (Op (first More) T Out))]
         [else
          (define-values (front back) (split-at More (/ N 2)))
          (Net (F B)
               (nary front F)
               (nary back B)
               (Op F B Out))])]))
  nary)

(define And* (binary->nary And TRUE))
(module+ test
  (chk-tt And*
          '((((0 0 0)) (0))
            (((0 0 1)) (0))
            (((0 1 0)) (0))
            (((0 1 1)) (0))
            (((1 0 0)) (0))
            (((1 0 1)) (0))
            (((1 1 0)) (0))
            (((1 1 1)) (1)))))

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
          '((((0 0 0)) (0))
            (((0 0 1)) (1))
            (((0 1 0)) (1))
            (((0 1 1)) (1))
            (((1 0 0)) (1))
            (((1 0 1)) (1))
            (((1 1 0)) (1))
            (((1 1 1)) (1))))
  (chk-tt Or*
          '((((0 0 0 0)) (0))
            (((0 0 0 1)) (1))
            (((0 0 1 0)) (1))
            (((0 0 1 1)) (1))
            (((0 1 0 0)) (1))
            (((0 1 0 1)) (1))
            (((0 1 1 0)) (1))
            (((0 1 1 1)) (1))
            (((1 0 0 0)) (1))
            (((1 0 0 1)) (1))
            (((1 0 1 0)) (1))
            (((1 0 1 1)) (1))
            (((1 1 0 0)) (1))
            (((1 1 0 1)) (1))
            (((1 1 1 0)) (1))
            (((1 1 1 1)) (1)))))

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

(define (Id/N src dst)
  (map Id src dst))
(module+ test
  (define-chk-num chk-idn
    #:N N #:in ([Src N]) #:out ([Dst N])
    #:circuit Id/N #:exhaust 5
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
       ;; I'm not sure how to generalize it like that. When I look up
       ;; decoder circuits in the literature, they seem to do this,
       ;; but I can't see what the pattern is.
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
  (Net ([CodeIn N] [CodeOut N])
       (Decoder/N CodeIn Os)
       (Increment/N CodeOut GROUND CodeIn)
       (Latch/N TRUE CodeIn CodeOut)))
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

(define (ROM-AddrSpace vals)
  (integer-length (sub1 (length vals))))

(define (number->bits N n)
  (for/list ([i (in-range N)])
    (bitwise-bit-set? n i)))
(define (numbers->bits N ns)
  (map (λ (n) (number->bits N n)) ns))

(define (ROM-1bit value-bits Which ValueBitOut)
  (unless (= (length value-bits) (length Which))
    (error 'ROM-1bit "Mismatch of signals and bits"))
  ;; If the value bit is #t, then connect it to the output or
  (Or* (for/list ([vb (in-list value-bits)]
                  [w (in-list Which)]
                  #:when vb)
         w)
       ValueBitOut))
(module+ test
  (chk-tt ROM-1bit
          '([((#f #t) (0 0)) (0)]
            [((#f #t) (1 0)) (0)]
            [((#f #t) (0 1)) (1)]

            [((#t #f) (0 0)) (0)]
            [((#t #f) (1 0)) (1)]
            [((#t #f) (0 1)) (0)]

            [((#f #f) (0 0)) (0)]
            [((#f #f) (1 0)) (0)]
            [((#f #f) (0 1)) (0)]

            [((#t #t) (0 0)) (0)]
            [((#t #t) (1 0)) (1)]
            [((#t #t) (0 1)) (1)])))

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
  (define val-bits (numbers->bits W vals))

  ;; So, we have a different circuit for each bit of the ROM's output
  (define set-value-net
    (for/list ([Value_i (in-list Value)]
               [i (in-naturals)])
      (ROM-1bit (map (λ (vw) (list-ref vw i)) val-bits)
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
  (Id/N FS FD))
(module+ test
  (chk-tt Cut/N
          '([(( 0 1  0  1 1 0  0 1))
             (((0 1) 0 (1 1 0) 0 1))])))

(define (check-RegisterArgs who Value Which Registers)
  (unless (= (length Which) (length Registers))
    (error who "Not enough selectors for register set"))

  (define ValueN (length Value))
  (for ([R (in-list Registers)])
    (unless (= (length R) ValueN)
      (error who "Bit mismatch in register/value"))))

;; xxx See if this can be improved based on the following system:
;; http://sce2.umkc.edu/csee/hieberm/281_new/lectures/seq-storage-components/seq-storage.html
(define (RegisterSet Signal In Which Registers)
  (check-RegisterArgs 'RegisterSet In Which Registers)

  (for/list ([W (in-list Which)]
             [R (in-list Registers)])
    (Net (Signal*W)
         (And Signal W Signal*W)
         (Latch/N Signal*W In R))))
(module+ test
  (let ()
    (define-wires Signal [In 3] [Which 2])
    (define Registers (build-list 2 (λ (i) (Bundle 3))))
    (simulate&chk
     (RegisterSet Signal In Which Registers)
     (flatten
      (list
       Signal   In     Which   Registers))
     '([(0      0 0 0  0 0     0 0 0  0 0 0)
        (0      0 0 0  0 0     _ _ _  _ _ _)
        (0      0 0 0  0 0     0 0 0  0 0 0)]

       [(0      0 0 0  0 0     0 0 0  0 0 0)
        (0      1 0 1  0 0     _ _ _  _ _ _)
        (0      1 0 1  0 0     0 0 0  0 0 0)]

       [(0      1 0 1  0 0     0 0 0  0 0 0)
        (1      1 0 1  0 0     _ _ _  _ _ _)
        (1      1 0 1  0 0     0 0 0  0 0 0)]

       [(1      1 0 1  0 0     0 0 0  0 0 0)
        (1      1 0 1  1 0     _ _ _  _ _ _)
        (1      1 0 1  1 0     1 0 1  0 0 0)]

       [(1      1 0 1  1 0     1 0 1  0 0 0)
        (1      0 0 1  0 1     _ _ _  _ _ _)
        (1      0 0 1  0 1     1 0 1  0 0 1)]

       [(1      0 0 1  0 1     1 0 1  0 0 1)
        (0      0 0 0  0 0     _ _ _  _ _ _)
        (0      0 0 0  0 0     1 0 1  0 0 1)]))))

(define (RegisterRead-1Bit RegisterBits Which OutBit)
  (check-RegisterArgs 'RegisterRead-1Bit (list OutBit) Which (map list RegisterBits))
  (Net ([R*W (length Which)])
       (And/N RegisterBits Which R*W)
       (Or* R*W OutBit)))
(module+ test
  (chk-tt
   RegisterRead-1Bit
   '([((0 0) (0 0)) (0)]
     [((1 0) (1 0)) (1)]
     [((0 1) (0 1)) (1)])))

(define (RegisterRead Registers Which Out)
  (check-RegisterArgs 'RegisterRead Out Which Registers)
  (for/list ([O (in-list Out)]
             [i (in-naturals)])
    (RegisterRead-1Bit
     (for/list ([R (in-list Registers)])
       (list-ref R i))
     Which
     O)))
(module+ test
  (chk-tt
   RegisterRead
   '([(((1 0) (0 1)) (0 0)) ((0 0))]
     [(((1 0) (0 1)) (1 0)) ((1 0))]
     [(((1 0) (0 1)) (0 1)) ((0 1))])))

(define (IsZero? In Bit)
  (Net (T)
       (Or* In T)
       (Not T Bit)))
(module+ test
  (define-chk-num chk-iszero
    #:N N #:in ([In N]) #:out (Bit)
    #:circuit IsZero? #:exhaust 5
    #:check
    (chk Bit (zero? In))))

(define (bits->number bs)
  (for/sum ([i (in-naturals)]
            [b (in-list bs)])
    (* (if b 1 0) (expt 2 i))))
(module+ test
  (chk (bits->number '(#t #f #f #f))  1
       (bits->number '(#t #f #t #f))  5
       (bits->number '(#t #t #f #t)) 11
       (bits->number '(#t #t #t #t)) 15))

(define (unsigned->signed bits x)
  (define unbs (number->bits bits x))
  (match-define (cons last-bit rfirst-bits) (reverse unbs))
  (define first-bits (reverse rfirst-bits))
  (+ (* -1 (if last-bit 1 0) (expt 2 (sub1 bits)))
     (bits->number first-bits)))
(module+ test
  (chk (unsigned->signed 4 #b0001)  1
       (unsigned->signed 4 #b0101)  5
       (unsigned->signed 4 #b1011) -5
       (unsigned->signed 4 #b1111) -1))

(define (IsNegative? In Bit)
  (Id (last In) Bit))
(module+ test
  (define-chk-num chk-isneg
    #:N N #:in ([In N]) #:out (Bit)
    #:circuit IsNegative? #:exhaust 5
    #:check
    (chk Bit (negative? (unsigned->signed N In)))))

(define-syntax-rule (debug-wires lab w ...)
  (debug
   (λ ()
     (printf "~a: ~v\n" lab (list (cons 'w (read-number w)) ...)))))

;; XXX Contracts and docs
(provide HDL-DEBUG?
         Mt Net
         define-wires Wire Bundle
         simulate!
         bread bwrite!
         breadn bwriten!
         read-number write-number!
         unsigned->signed bits->number
         TRUE FALSE GROUND
         Nand
         
         Latch Latch/N
         Not Not/N
         Id Id/N
         And And/N And/wb And*
         Or Or*
         Nor Xor
         Xnor
         Mux Mux/N
         Demux
         Half-Adder Full-Adder Adder/N
         Increment/N
         Shifter Shifter/N
         Decoder Decoder/N
         Clock
         ROM-AddrSpace ROM
         Cut/N
         RegisterSet
         RegisterRead
         IsZero? IsNegative?)
(module+ test
  (provide chk-tt define-chk-num simulate&chk))
