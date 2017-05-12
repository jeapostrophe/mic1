#lang info
(define version "1.0")
(define deps (list))
(define build-deps (list))
(define collection "mic1")

;; xxx scribblings

(define raco-commands
  '(("mic1" (submod mic1/mic1 main) "run MIC1 simulator" #f)
    ("mcc" (submod mic1/mcc main) "run MIC1 microcode compiler" #f)
    ("masm" (submod mic1/masm main) "run MIC1 macroassembler" #f)))
