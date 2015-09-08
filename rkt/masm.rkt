#lang racket/base

;; Assembler Syntax
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax-rule (mic1-block inst ...)
  (compile-block (parse-block inst ...)))
(define-syntax-rule (%define lab)
  (raise-syntax-error '%define "Illegal outside mic1-block"))
(define-syntax (parse-block stx)
  (syntax-parse stx
    [(_ inst ...)
     (with-syntax
       ([(lab ...)
         (filter (λ (x) x)
                 (for/list ([inst (syntax->list #'(inst ...))])
                   (syntax-parse inst
                     #:literals (%define)
                     [(%define lab)
                      #'lab]
                     [_
                      #f])))])
       (syntax/loc stx
         (let ([lab (label)] ...)
           (list (parse-inst inst) ...))))]))
(define-syntax (parse-inst stx)
  (syntax-parse stx
    #:literals (%define %relocate)
    [(_ (~and meta (%define lab)))
     (syntax/loc #'meta
       (%label-def! lab))]
    [(_ (~and meta (%relocate . _)))
     #'meta]
    [(_ raw)
     (syntax/loc #'raw
       (%inst (λ () raw)))]))

(struct label ())

;; Assembler
(require racket/match
         racket/list
         racket/format)

(struct %meta ())
(struct %label-def! %meta (lab))
(struct %relocate (loc))
(struct %inst (gen))

(define current-label->loc
  (make-parameter #f))

(define (compile-block metas)
  (parameterize ([current-label->loc (compute-labels metas)])
    (emit 0 metas))
  (void))

(define (compute-labels metas)
  (define label->loc (make-hasheq))
  (for/fold ([loc 0])
            ([m (in-list metas)])
    (match m
      [(%label-def! lab)
       (hash-set! label->loc lab loc)
       loc]
      [(%relocate loc)
       loc]
      [_
       (+ loc 1)]))
  label->loc)

(define (emit loc metas)
  (match metas
    ['()
     loc]
    [(cons m ms)
     (emit (emit loc m) ms)]
    [(%meta)
     loc]
    [(%relocate dest-loc)
     (emit loc (make-list (- dest-loc loc) "1111111111111111"))]
    [(%inst g)
     (emit loc (g))]
    [(? string? s)
     (displayln s)
     (+ loc 1)]
    [x
     (displayln (->bits 16 x))
     (+ loc 1)]))

(define (->bits width value)
  (match value
    [(? label? lab)
     (->bits width
             (hash-ref (current-label->loc) lab
                       (λ ()
                         (error 'mic1-block "Unknown label"))))]
    [(? number? n)
     (~a #:min-width width #:align 'right #:pad-string "0" (number->string n 2))]))

(define (lodd x) (format "0000~a" (->bits 12 x)))
(define (stod x) (format "0001~a" (->bits 12 x)))
(define (addd x) (format "0010~a" (->bits 12 x)))
(define (subd x) (format "0011~a" (->bits 12 x)))
(define (jpos x) (format "0100~a" (->bits 12 x)))
(define (jzer x) (format "0101~a" (->bits 12 x)))
(define (jump x) (format "0110~a" (->bits 12 x)))
(define (loco x) (format "0111~a" (->bits 12 x)))
(define (lodl x) (format "1000~a" (->bits 12 x)))
(define (stol x) (format "1001~a" (->bits 12 x)))
(define (addl x) (format "1010~a" (->bits 12 x)))
(define (subl x) (format "1011~a" (->bits 12 x)))
(define (jneg x) (format "1100~a" (->bits 12 x)))
(define (jnze x) (format "1101~a" (->bits 12 x)))
(define (call x) (format "1110~a" (->bits 12 x)))
(define (pshi  ) (format "1111000000000000"))
(define (popi  ) (format "1111001000000000"))
(define (push  ) (format "1111010000000000"))
(define (pop   ) (format "1111011000000000"))
(define (retn  ) (format "1111100000000000"))
(define (swap  ) (format "1111101000000000"))
(define (insp y) (format "11111100~a" (->bits 8 y)))
(define (desp y) (format "11111110~a" (->bits 8 y)))
(define (halt  ) (format "1111111100000000"))

;; Example
(module+ main
  (mic1-block
   (%define start:)
   (lodd daddr:)              ;; load AC with data address
   (push)                     ;; push AC to stack (2nd arg)
   (lodd dcnt:)               ;; load AC with data count
   (push)                     ;; push AC to stack (1st arg)
   (call adder:)              ;; push return address on stack
   (stod rslt:)               ;; store AC (has sum) to rslt: location
   (halt)                     ;; enter debugger
   (%define daddr:)
   data:                      ;; location holds data array address
   (%define data:)
   25                         ;; first of 5 data values
   50
   75
   100
   125                        ;; last of 5 data values
   (%define dcnt:)
   5                          ;; location holds data array element count
   (%define rslt:)
   0                          ;; location for the sum to be stored
   (%relocate 20)             ;; forces adder routine to start at location 20
   (%define adder:)
   (lodl 1)                   ;; get 1st arg from stack into AC (data count)
   (stod mycnt:)              ;; store count at location mycnt:
   (lodl 2)                   ;; get 2nd arg from stack into AC (data addr)
   (pshi)                     ;; push indirect first datum to stack
   (addd myc1:)               ;; add 1 (value at myc1:) to addr in AC
   (stod myptr:)              ;; store new addr to location myptr:
   (%define loop:)
   (lodd mycnt:)              ;; load AC with value at mycnt: (data count)
   (subd myc1:)               ;; subtract 1 (value at myc1:) from AC
   (jzer done:)               ;; if new data count is 0 go to location done:
   (stod mycnt:)              ;; if more data to add, store new data count
   (lodd myptr:)              ;; load AC with addr of next datum
   (pshi)                     ;; push indirect next datum to stack
   (addd myc1:)               ;; add 1 (value at myc1:) to addr in AC
   (stod myptr:)              ;; store new addr to location myptr:
   (pop)                      ;; pop top of stack into AC (new datum)
   (addl 0)                   ;; add new top of stack location to AC
   (insp 1)                   ;; move stack pointer down one place
   (push)                     ;; push new sum in AC onto stack
   (jump loop:)               ;; jump to location loop:
   (%define done:)
   (pop)                      ;; come here when all data added, sum in AC
   (retn)                     ;; return to caller
   (halt)                     ;; should never get here (safety halt)
   (%define mycnt:)
   0                          ;; location for running count
   (%define myptr:)
   0                          ;; location for running data pointer
   (%define myc1:)
   1                          ;; location of a constant value of 1
   ))
