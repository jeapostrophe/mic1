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
(define DEBUG? (make-parameter #f))
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
     [(debug f) (when (DEBUG?) (f))]
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
  (Dupe/N FS FD))
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
               (flatten
                (list MIR:AMUX MIR:COND MIR:ALU MIR:SH
                      MIR:MBR MIR:MAR MIR:RD MIR:WR
                      MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR))))
       (debug-wires "post-Cut"
                    MIR:AMUX MIR:COND MIR:ALU MIR:SH
                    MIR:MBR MIR:MAR MIR:RD MIR:WR
                    MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR)
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
       (debug-wires "post ALU" Amux-out B-Bus MIR:ALU ALU-out N Z)
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

(define-syntax-rule (debug-wires lab w ...)
  (debug (λ () (printf "~a: ~v\n" lab (list (cons 'w (read-number w)) ...)))))

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

(define simulator-registers
  '(PC AC SP IR TIR Z P1 N1 AMASK SMASK A B C D E F))
(define simulator-vars
  (append '(MPC Read? Write? MAR MBR) simulator-registers))

(define (make-MIC1-simulator
         MicrocodeImage MemoryImage InitialPC InitialSP)
  (define WordSize 16)
  (define RegisterCount 16)
  (define MicrocodeSize 256)
  (define MicrocodeWordSize 32)

  (when (DEBUG?)
    (printf "Img ~e\n" MicrocodeImage))
  (define MicrocodeVec
    (image->memory MicrocodeSize MicrocodeWordSize MicrocodeImage))
  (define Microcode
    (vector->list MicrocodeVec))
  (when (DEBUG?)
    (printf "Code ~e\n" Microcode))

  (define-wires
    [MPC (ROM-AddrSpace Microcode)]
    Read? Write?
    [MAR WordSize]
    [MBR WordSize])
  (define Registers
    (build-list RegisterCount (λ (i) (Bundle WordSize))))
  (define WireMap
    (for/hasheq ([label
                  (in-list simulator-vars)]
                 [reg (in-list (list* MPC Read? Write? MAR MBR Registers))])
      (values label reg)))

  (define (register-set! r n)
    (write-number! (hash-ref WireMap r) n))
  (define (register-read r)
    (read-number (hash-ref WireMap r)))

  (register-set! 'PC InitialPC)
  (register-set! 'SP InitialSP)
  (register-set! 'Z 0)
  (register-set! 'P1 +1)
  (register-set! 'N1 -1)
  (register-set! 'AMASK #b0000111111111111)
  (register-set! 'SMASK #b0000000011111111)

  (define the-mic1
    (MIC1 MicrocodeWordSize Microcode
          Registers MPC
          Read? Write?
          MAR MBR))

  (analyze #:label "MIC1" the-mic1)

  (define Memory
    ;; Image is smaller because there are 4 bits in instructions. This
    ;; could be removed with memory banking or by allowing the stack
    ;; to be higher, etc.
    (image->memory (expt 2 (- WordSize 4)) WordSize MemoryImage))

  (simulator
   MicrocodeVec Memory register-set! register-read
   (λ (inform!)
     (let loop ([readc 0] [writec 0])
       (simulate! the-mic1)
       (inform!)
       ;; xxx implement IO
       (define next-readc (if (bread Read?) (add1 readc) 0))
       (define next-writec (if (bread Write?) (add1 writec) 0))
       (when (> next-writec 4)
         (vector-set! Memory (read-number MAR) (read-number MBR))
         (set! next-writec 0))
       (when (> next-readc 4)
         (write-number! MBR (vector-ref Memory (read-number MAR)))
         (set! next-readc 0))
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

(define (μwrite ns)
  (define-wires
    [MIR 32]
    MIR:AMUX [MIR:COND 2] [MIR:ALU 2] [MIR:SH 2]
    MIR:MBR MIR:MAR MIR:RD MIR:WR MIR:ENC
    [MIR:C 4] [MIR:B 4] [MIR:A 4]
    [MIR:ADDR 8])
  (define fields
    (list MIR:AMUX MIR:COND MIR:ALU MIR:SH
          MIR:MBR MIR:MAR MIR:RD MIR:WR
          MIR:ENC MIR:C MIR:B MIR:A MIR:ADDR))
  (for-each write-number! fields ns)
  (simulate! (Cut/N (reverse (flatten fields)) MIR))
  (read-number MIR))

(module+ test
  (define standard-reg-values
    (for/hasheq ([rn (in-list '(PC AC SP IR TIR A B C D E F))]
                 [v (in-naturals 10)])
      (values rn v)))
  (define (chk-mic1μ μinst-sym before after)
    (define μinst-ns (μencode μinst-sym))
    (define μinst (μwrite μinst-ns))
    (define s (make-MIC1-simulator (list μinst) empty 0 1024))
    (match-define (simulator Microcode Memory r! r start!) s)

    (for ([(rn n) (in-hash standard-reg-values)])
      (r! rn n))

    (for ([(rn n) (in-hash before)])
      (r! rn n))

    (define init
      (for/hasheq ([sv (in-list simulator-vars)])
        (values sv (r sv))))

    (let/ec esc
      (define c 0)
      (define (inform!)
        (set! c (add1 c))
        (when (= c 4)
          (esc)))
      (start! inform!))

    (with-chk (['μinst-sym μinst-sym]
               ['before before])
      ;; Everything not in after stayed the same
      (for ([(sv svb) (in-hash init)]
            #:unless (hash-has-key? after sv))
        (with-chk (['sv sv]
                   ['svb svb])
          (chk (r sv) svb)))
      ;; Everything in after got the new value
      (for ([(rn n) (in-hash after)])
        (with-chk (['rn rn])
          (chk (r rn) n)))))

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

;; xxx make a FSM/RTL version of the MIC1

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

(define (file->image p)
  (local-require racket/file)
  (define lines (file->lines p))
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

;; xxx individual microinstructions work correctly, but programs are
;; not running correctly.

(module+ main
  (require racket/runtime-path
           racket/cmdline)

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
