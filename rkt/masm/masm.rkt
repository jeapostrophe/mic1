#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list
         racket/format)

;; Syntax

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
         (let ([lab (label 'lab)] ...)
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

;; Assembler

(struct label (tag))

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
     (emit loc (make-list (- dest-loc loc) fill))]
    [(%inst g)
     (emit loc (g))]
    [(? string? s)
     (displayln s)
     (+ loc 1)]
    [x
     (displayln (->bits 16 x))
     (+ loc 1)]))

(define fill "1111111111111111")

(define (->bits width value)
  (match value
    [(label tag)
     (->bits width
             (hash-ref (current-label->loc) value
                       (λ ()
                         (error 'mic1-block "label(~a) does not have location"
                                tag))))]
    [(? number? n)
     (~a #:min-width width #:align 'right #:pad-string "0" (number->string n 2))]))

;; Instructions

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
(define pshi "1111000000000000")
(define popi "1111001000000000")
(define push "1111010000000000")
(define pop  "1111011000000000")
(define retn "1111100000000000")
(define swap "1111101000000000")
(define (insp y) (format "11111100~a" (->bits 8 y)))
(define (desp y) (format "11111110~a" (->bits 8 y)))
(define halt "1111111100000000")

;; Interface

(provide mic1-block
         
         %define %relocate
         
         lodd stod addd subd jpos jzer jump loco lodl
         stol addl subl jneg jnze call pshi popi push
         pop retn swap insp desp halt)
