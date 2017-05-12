#lang racket/base
(require racket/contract/base)

;; xxx write this

(define (asm->image/file p)
  (error 'asm->image/file "xxx"))

(provide
 (contract-out
  [asm->image/file
   (-> path-string? (listof exact-nonnegative-integer?))]))
