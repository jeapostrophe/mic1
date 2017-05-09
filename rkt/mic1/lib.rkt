#lang racket/base
(require racket/match
         racket/contract/base)
(module+ test
  (require chk))

(define (number->bits N n)
  (for/list ([i (in-range N)])
    (bitwise-bit-set? n i)))
(define (numbers->bits N ns)
  (map (λ (n) (number->bits N n)) ns))

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

(provide
 (contract-out
  [unsigned->signed
   (-> exact-positive-integer? exact-integer?
       exact-integer?)]
  [bits->number
   (-> (listof boolean?)
       exact-integer?)]
  [numbers->bits
   (-> exact-integer? (listof exact-integer?)
       (listof (listof boolean?)))]))
