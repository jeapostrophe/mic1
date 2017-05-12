#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         syntax/readerr
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "simulator.rkt")
(module+ test
  (require chk)
  (require racket/runtime-path)
  (define-runtime-path macro-v1.mc
    "../../examples/macro-v1.mc"))

(struct alu-op () #:transparent)
(struct alu-plus alu-op (a-bus b-bus) #:transparent)
(struct alu-id alu-op (a-bus) #:transparent)
(struct alu-band alu-op (a-bus b-bus) #:transparent)
(struct alu-inv alu-op (a-bus) #:transparent)

(struct sh-op (alu) #:transparent)
(struct sh-id sh-op () #:transparent)
(struct sh-lshift sh-op () #:transparent)
(struct sh-rshift sh-op () #:transparent)

(struct mc-comp () #:transparent)
(struct mc-setc mc-comp (c-bus sh) #:transparent)
(struct mc-alu mc-comp (alu) #:transparent)
(struct mc-mar mc-comp (b-bus) #:transparent)
(struct mc-if mc-comp (cond addr) #:transparent)
(struct mc-rd mc-comp () #:transparent)
(struct mc-wr mc-comp () #:transparent)

;; xxx include srcloc
(struct mc-inst (comps) #:transparent)
(struct mc-label (lab) #:transparent)

(define-tokens MC-TOKS (LABEL REG))
(define-tokens MC-MT-TOKS
  (EOF
   MAR MBR ALU
   SEMI SET NL WR RD IF THEN GOTO COND_N COND_Z
   LSHIFT LPAREN RPAREN RSHIFT PLUS INV BAND COMMA COLON))
(define mc-lex
  (lexer-src-pos
   [(eof) 'EOF]
   [(:or "pc" "ac" "sp" "ir" "tir" "0" "1" "(-1)" "amask" "smask"
         "a" "b" "c" "d" "e" "f")
    (token-REG lexeme)]
   ["mar" 'MAR]
   ["mbr" 'MBR]
   ["alu" 'ALU]
   ["inv" 'INV]
   ["band" 'BAND]
   ["lshift" 'LSHIFT]
   ["rshift" 'RSHIFT]
   ["+" 'PLUS]
   ["," 'COMMA]
   ["(" 'LPAREN]
   [")" 'RPAREN]
   ["wr" 'WR]
   ["rd" 'RD]
   ["if" 'IF]
   ["then" 'THEN]
   ["goto" 'GOTO]
   ["n" 'COND_N]
   ["z" 'COND_Z]
   [":=" 'SET]
   ["\n" 'NL]
   [";" 'SEMI]
   [":" 'COLON]
   [(:+ (:- any-char (:or #\{ "\n" " " "(" "," ")" #\: #\;))) (token-LABEL lexeme)]
   [(:or #\tab #\space) (return-without-pos (mc-lex input-port))]
   [#\{
    (begin
      (let loop ()
        (define c (read-char input-port))
        (unless (or (eof-object? c)
                    (char=? c #\}))
          (loop)))
      (return-without-pos (mc-lex input-port)))]))
(module+ test
  #;(chk
     (map position-token-token
          (call-with-input-file macro-v1.mc
            (λ (ip)
              (for/list ([i (in-range 25)]) (mc-lex ip)))))
     empty))

(define (mc-parse source-name)
  (parser
   (src-pos)
   (tokens MC-TOKS MC-MT-TOKS)
   (start Program)
   (end EOF)
   (error
    (lambda (a name val start end)
      (raise-read-error
       (format "mc-parse: unexpected token ~a~a" name
               (if val (format "(~v)" val) ""))
       source-name
       (position-line start)
       (position-col start)
       (position-offset start)
       (- (position-offset end)
          (position-offset start)))))

   (grammar
    (Program [() empty]
             [(NL Program) $2]
             [(LABEL COLON NL Program) (cons (mc-label $1) $4)]
             [(Instruction NL Program) (cons (mc-inst $1) $3)])
    (Instruction [() empty]
                 [(InstComp SEMI Instruction)
                  (cons $1 $3)])
    (InstComp [(MAR SET REG) (mc-mar $3)]
              [(CExpr SET ShExpr) (mc-setc $1 $3)]
              [(ALU SET AluExpr) (mc-alu $3)]
              [(IF Cond THEN GOTO LABEL) (mc-if $2 $5)]
              [(GOTO LABEL) (mc-if 'J! $2)]
              [(RD) (mc-rd)]
              [(WR) (mc-wr)])
    (Cond [(COND_N) 'JN]
          [(COND_Z) 'JZ])
    (ShExpr [(AluExpr) (sh-id $1)]
            [(LSHIFT LPAREN AluExpr RPAREN) (sh-lshift $3)]
            [(RSHIFT LPAREN AluExpr RPAREN) (sh-rshift $3)])
    (AluExpr [(AExpr PLUS BExpr) (alu-plus $1 $3)]
             [(AExpr) (alu-id $1)]
             [(INV LPAREN AExpr RPAREN) (alu-inv $3)]
             [(BAND LPAREN AExpr COMMA BExpr RPAREN) (alu-band $3 $5)])
    (AExpr [(REG) $1]
           [(MBR) 'mbr])
    (BExpr [(REG) $1])
    (CExpr [(REG) $1]
           [(MBR) 'mbr]))))

(define (microcode-parse ip)
  (port-count-lines! ip)
  ((mc-parse (object-name ip)) (λ () (mc-lex ip))))

(define (hash-set1 ht k v)
  (define (up old)
    (if old
      (if (eq? old v)
        v
        (error 'μcompile "Incompatible ~a: ~a vs ~a" k old v))
      v))
  (hash-update ht k up #f))

(define (μcompile-reg L reg which)
  (hash-set1 L reg
             (match which
               ["pc" 'PC] ["ac" 'AC] ["sp" 'SP] ["ir" 'IR] ["tir" 'TIR]
               ["0" 'Z] ["1" 'P1] ["(-1)" 'N1] ["amask" 'AMASK] ["smask" 'SMASK]
               ["a" 'A] ["b" 'B] ["c" 'C] ["d" 'D] ["e" 'E] ["f" 'F])))
(define (μcompile-b-bus L b-bus)
  (μcompile-reg L 'B b-bus))
(define (μcompile-c-bus L c-bus)
  (match c-bus
    ['mbr
     (hash-set1 L 'MBR 'MBR)]
    [_
     (μcompile-reg L 'C c-bus)]))
(define (μcompile-a-bus L a-bus)
  (match a-bus
    ['mbr
     (hash-set1 L 'AMUX 'MBR)]
    [_
     (μcompile-reg L 'A a-bus)]))
(define (μcompile-alu L alu)
  (match alu
    [(alu-plus a-bus b-bus)
     (hash-set1 (μcompile-b-bus (μcompile-a-bus L a-bus) b-bus) 'ALU '+)]
    [(alu-id a-bus)
     (hash-set1 (μcompile-a-bus L a-bus) 'ALU 'A)]
    [(alu-band a-bus b-bus)
     (hash-set1 (μcompile-b-bus (μcompile-a-bus L a-bus) b-bus) 'ALU '&)]
    [(alu-inv a-bus)
     (hash-set1 (μcompile-a-bus L a-bus) 'ALU '!)]))
(define (μcompile-sh L sh)
  (match-define (sh-op alu) sh)
  (hash-set1
   (μcompile-alu L alu) 'SH
   (match sh
     [(sh-id _) 'NS]
     [(sh-lshift _) 'LS]
     [(sh-rshift _) 'RS])))
(define (μcompile-comp label->idx L comp)
  (match comp
    [(mc-setc c-bus sh)
     (hash-set1 (μcompile-c-bus (μcompile-sh L sh) c-bus) 'ENC 'ENC)]
    [(mc-alu alu)
     (μcompile-alu L alu)]
    [(mc-mar b-bus)
     (hash-set1 (μcompile-b-bus L b-bus) 'MAR 'MAR)]
    [(mc-if cond label)
     (hash-set1 (hash-set1 L 'COND cond)
                'ADDR
                (hash-ref label->idx label
                          (λ ()
                            (error 'μcompile "Unknown label: ~v" label))))]
    [(mc-rd) (hash-set1 L 'RD 'RD)]
    [(mc-wr) (hash-set1 L 'WR 'WR)]))

(define (μcompile label->idx m)
  (define DEFAULT
    (hasheq 'AMUX 'A  'COND 'NJ
            'ALU  '+  'SH   'NS
            'MBR  'NB 'MAR  'NA
            'RD   'NR 'WR   'NW 'ENC 'NC
            'C    'PC 'B    'PC 'A   'PC 'ADDR 0))
  (define COMPILED
    (for/fold ([L (hasheq)]) ([comp (in-list (mc-inst-comps m))])
      (μcompile-comp label->idx L comp)))
  (define FINAL
    (for/hasheq ([(k def) (in-hash DEFAULT)])
      (values k (or (hash-ref COMPILED k #f) def))))
  (map (λ (k) (hash-ref FINAL k))
       '(AMUX COND ALU SH MBR MAR RD WR ENC C B A ADDR)))
(module+ test
  (define std (hash "START" 0 "END" 100))
  (define-syntax-rule (chk-μc [in out] ...)
    (begin (chk (μcompile std in) out) ...))
  (chk-μc [(mc-inst (list (mc-mar "pc") (mc-rd)))
           (list 'A 'NJ '+ 'NS 'NB 'MAR 'RD 'NW 'NC 'PC 'PC 'PC 0)]))

(define (microcode->microcode-image mc)
  (define-values (label->idx max-addr)
    (for/fold ([ht (hash)] [i 0])
              ([m (in-list mc)])
      (match m
        [(mc-label lab)
         (values (hash-set ht lab i) i)]
        [(mc-inst _)
         (values ht (add1 i))])))
  (for/list ([m (in-list mc)]
             #:when (mc-inst? m))
    (μwrite (μencode (μcompile label->idx m)))))

(define (microcode->microcode-image/port ip)
  (microcode->microcode-image (microcode-parse ip)))

(define (microcode->microcode-image/file p)
  (call-with-input-file p microcode->microcode-image/port))

(define (main!)
  (local-require racket/cmdline)
  (command-line
   #:program "mcc"
   #:args (microcode-path)
   (with-output-to-file (path-replace-extension microcode-path #".prom")
     #:exists 'replace
     (λ ()
       (for-each
        displayln
        (image->lines
         MicrocodeWordSize
         (microcode->microcode-image/file
          microcode-path)))))))
(module+ main
  (main!))
(module+ test
  (require racket/file)
  (define-runtime-path macro-v1.prom.expected
    "../../examples/macro-v1.prom.expected")
  (parameterize ([current-command-line-arguments
                  (vector (path->string macro-v1.mc))])
    (main!))
  (define macro-v1.prom
    (path-replace-extension macro-v1.mc #".prom"))
  (for ([actual (in-list (file->lines macro-v1.prom))]
        [expected (in-list (file->lines macro-v1.prom.expected))]
        [i (in-naturals)])
    (with-chk (['i i]) (chk actual expected))))

(provide
 (contract-out
  [microcode->microcode-image/file
   (-> path-string? (listof exact-nonnegative-integer?))]))
