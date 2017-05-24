#lang info
(define version "1.0")

(define deps (list "base"
                   "parser-tools-lib"))
(define build-deps (list "racket-doc"
                         "scribble-lib"
                         "chk"))

(define collection "mic1")

(define scribblings '(("mic1.scrbl" () ("Hardware"))))

(define raco-commands
  '(("mic1" (submod mic1/mic1 main) "run MIC1 simulator" #f)
    ("mcc" (submod mic1/mcc main) "run MIC1 microcode compiler" #f)
    ("masm" (submod mic1/masm main) "run MIC1 macroassembler" #f)))

