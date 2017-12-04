#!/usr/bin/env racket
#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         syntax/readerr
         parser-tools/yacc
         parser-tools/lex
         racket/format
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base
                     syntax/parse)
         "lib.rkt"
         "simulator.rkt")
(module+ test
  (require chk))

(define-tokens AS-TOKS (LABEL NUM STR))
(define-tokens AS-MT-TOKS (EOF LOC))

(define-syntax (define-asm-lexer stx)
  (syntax-parse stx
    [(_ (AS-INST-TOKS:id asm-lex:id)
        #:instructions inst:id ...
        #:lexer lexer-action ...)
     (with-syntax
         ([(inst-action ...)
           (for/list ([inst (in-list (syntax-e #'(inst ...)))])
             (with-syntax
                 ([(char ...)
                   (for/list ([c (in-string (symbol->string (syntax->datum inst)))])
                     #`(:or #,(char-upcase c) #,(char-downcase c)))])
               (quasisyntax/loc inst
                 [(:: char ...) '#,inst])))])
       (syntax/loc stx
         (begin
           (define-tokens AS-INST-TOKS (inst ...))
           (define asm-lex
             (lexer-src-pos
              inst-action ...
              lexer-action ...)))))]))

(define-asm-lexer
  (AS-INST-TOKS asm-lex)
  #:instructions
  LODD STOD ADDD SUBD JPOS JZER JUMP LOCO
  LODL STOL ADDL SUBL JNEG JNZE CALL PSHI
  POPI PUSH POP  RETN SWAP INSP DESP HALT
  #:lexer
  [(eof) 'EOF]
  [(:or #\tab #\space #\newline) (return-without-pos (asm-lex input-port))]
  [(:: alphabetic (:* (:or alphabetic numeric "_")) #\:) (token-LABEL lexeme)]
  [(:: #\- (:+ numeric)) (token-NUM (string->number lexeme))]
  [(:+ numeric) (token-NUM (string->number lexeme))]
  [".LOC" 'LOC]
  [#\;
   (begin
     (let loop ()
       (define c (read-char input-port))
       (unless (or (eof-object? c)
                   (char=? c #\newline))
         (loop)))
     (return-without-pos (asm-lex input-port)))]
  [(:: #\" (complement (:: any-string #\" any-string) ) #\")
   (token-STR (substring lexeme 1 (sub1 (string-length lexeme))))])

(struct as-ldef (lab) #:transparent)
(struct as-lref (lab) #:transparent)
(struct as-rloc (num) #:transparent)
(struct as-inst (str) #:transparent)
(struct as-iarg (str arg) #:transparent)
(struct as-lnum (num) #:transparent)
(struct as-lstr (str) #:transparent)

(define asm-parser
  (parser
   (src-pos)
   (tokens AS-TOKS AS-MT-TOKS AS-INST-TOKS)
   (start Program)
   (end EOF)
   (error
    (lambda (a name val start end)
      (raise-read-error
       (format "asm-parserc-p: unexpected token ~a~a" name
               (if val (format "(~v)" val) ""))
       (file-path)
       (position-line start)
       (position-col start)
       (position-offset start)
       (- (position-offset end)
          (position-offset start)))))

   (grammar
    (Program [() empty]
             [(Inst Program) (cons $1 $2)])
    (Arg [(NUM) $1] [(LABEL) (as-lref $1)])
    (Inst [(LABEL   ) (as-ldef $1)]
          [(     NUM) (as-lnum $1)]
          [(     STR) (as-lstr $1)]
          [( LOC NUM) (as-rloc $2)]
          [(LODD Arg) (as-iarg "0000" $2)]
          [(STOD Arg) (as-iarg "0001" $2)]
          [(ADDD Arg) (as-iarg "0010" $2)]
          [(SUBD Arg) (as-iarg "0011" $2)]
          [(JPOS Arg) (as-iarg "0100" $2)]
          [(JZER Arg) (as-iarg "0101" $2)]
          [(JUMP Arg) (as-iarg "0110" $2)]
          [(LOCO Arg) (as-iarg "0111" $2)]
          [(LODL Arg) (as-iarg "1000" $2)]
          [(STOL Arg) (as-iarg "1001" $2)]
          [(ADDL Arg) (as-iarg "1010" $2)]
          [(SUBL Arg) (as-iarg "1011" $2)]
          [(JNEG Arg) (as-iarg "1100" $2)]
          [(JNZE Arg) (as-iarg "1101" $2)]
          [(CALL Arg) (as-iarg "1110" $2)]
          [(PSHI    ) (as-inst "1111000000000000")]
          [(POPI    ) (as-inst "1111001000000000")]
          [(PUSH    ) (as-inst "1111010000000000")]
          [( POP    ) (as-inst "1111011000000000")]
          [(RETN    ) (as-inst "1111100000000000")]
          [(SWAP    ) (as-inst "1111101000000000")]
          [(INSP Arg) (as-iarg "11111100" $2)]
          [(DESP Arg) (as-iarg "11111110" $2)]
          [(HALT    ) (as-inst "1111111100000000")]))))

(define (asm-parse ip)
  (port-count-lines! ip)
  (define on (object-name ip))
  (parameterize ([file-path on])
    (asm-pass1 (asm-parser (λ () (asm-lex ip))))))

(define (asm-pass1 l)
  (match l
    [(list) empty]
    [(list* (and x (as-ldef _)) (as-ldef y) z)
     (list* x (as-lref y) (asm-pass1 z))]
    [(cons x y)
     (cons x (asm-pass1 y))]))

(define (fmt-num len n)
  (list->string (map (λ (x) (if x #\1 #\0)) (reverse (number->bits len n)))))

(define (as-compile-arg label->idx len a)
  (match a
    [(? number?) (fmt-num len a)]
    [(as-lref lab)
     (fmt-num len
              (hash-ref label->idx lab
                        (λ () (error 'masm "Undefined label: ~v" lab))))]))

(define (as-compile label->idx reloc->len as)
  (match as
    [(as-ldef _) empty]
    [(as-lref lab) (as-compile-arg label->idx WordSize as)]
    [(as-lnum n) (fmt-num WordSize n)]
    [(as-iarg prefix arg)
     (string-append prefix
                    (as-compile-arg label->idx
                                    (- WordSize (string-length prefix))
                                    arg))]
    [(as-inst s) s]
    [(as-rloc _)
     (define len (hash-ref reloc->len as))
     (make-list len "1111111111111111")]
    [(as-lstr s)
     (let loop ([cs (map char->integer (string->list s))])
       (match cs
         [(list) empty]
         [(list x) (list (fmt-num WordSize x))]
         [(list* x y r)
          (cons (fmt-num WordSize (+ x (arithmetic-shift y 8))) (loop r))]))]))

(define (asm->symtab+image las)
  (define label->idx (make-hash))
  (define reloc->len (make-hasheq))
  (for/fold ([i 0])
            ([as (in-list las)])
    (match as
      [(as-ldef lab)
       (hash-set! label->idx lab i)
       i]
      [(as-rloc num)
       (unless (<= i num)
         (error 'masm "Can only relocate forward: ~v vs ~v" i num))
       (hash-set! reloc->len as (- num i))
       num]
      [(as-lstr str)
       (define len (string-length str))
       (+ i (if (even? len) len (add1 len)))]
      [_
       (+ i 1)]))
  (values
   (for/list ([lab (in-list (sort (hash-keys label->idx) string<=?))])
     (define i (hash-ref label->idx lab))
     (~a "# " (~a lab #:min-width 25 #:align 'left)
         " " (~a i #:min-width 4 #:align 'right)))
   (map
    (λ (i) (string->number i 2))
    (flatten
     (map
      (λ (as) (as-compile label->idx reloc->len as))
      las)))))

(define (asm->symtab+image/port ip)
  (asm->symtab+image (asm-parse ip)))

(define (asm->symtab+image/file p)
  (call-with-input-file p asm->symtab+image/port))

(define (main!)
  (local-require racket/cmdline
                 raco/command-name)
  (command-line
   #:program (short-program+command-name)
   #:args (asm-path)
   (with-output-to-file (path-replace-extension asm-path #".o")
     #:exists 'replace
     (λ ()
       (define-values (symtab image)
         (asm->symtab+image/file asm-path))
       (for-each displayln symtab)
       (for-each displayln (image->lines WordSize image))))))
(module+ main
  (main!))
(module+ test
  (require racket/file)

  (define (check-compilation as)
    (parameterize ([current-command-line-arguments
                    (vector (path->string as))])
      (main!))
    (define as.o
      (path-replace-extension as #".o"))
    (with-chk (['as as])
      (for ([actual (in-list (file->lines as.o))]
            [expected (in-list (file->lines
                                (path-replace-extension as
                                                        #".o.expected")))]
            [i (in-naturals)])
        (with-chk (['i i]) (chk actual expected)))))

  (require racket/runtime-path)
  (define-syntax-rule (static-check-compilation p)
    (begin (define-runtime-path tmp p)
           (when (file-exists? tmp)
             (check-compilation tmp))))
  (static-check-compilation "../examples/test.s")
  (static-check-compilation "../examples/IO_str_and_echo.s")
  (static-check-compilation "../examples/adder.s"))

(provide
 (contract-out
  [asm->symtab+image/file
   (-> path-string?
       (values (listof string?)
               (listof exact-nonnegative-integer?)))]))
