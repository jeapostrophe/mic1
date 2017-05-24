#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     mic1/mic1)
          scribble/bnf)

@title{MIC-1}
@author{Jay McCarthy}

The @code{mic1} package provides tools for working with the
@link["https://en.wikipedia.org/wiki/MIC-1"]{MIC-1 processor
architecture} that appears in Andrew S. Tanenbaum's textbook
@italic{Structured Computer Organization}.

@local-table-of-contents[]

@section{MIC-1 Description}

The MIC-1 is a CPU with 16 general purpose 16-bit registers. Registers
5, 6, 7, 8, and 9 have default values @litchar{0000000000000000},
@litchar{0000000000000001}, @litchar{1111111111111111},
@litchar{0000111111111111}, and @litchar{0000000011111111}
respectively.

It runs a single 256-instruction microprogram embedded in a control
store ROM. Its ALU supports addition, bitwise AND, and bitwise
negation. The ALU outputs flags for whether its result was negative or
zero. The ALU is connected to a 1-bit shifter that can shift left,
right, or not at all.

Its memory interface is two flags (one for reading, one for writing)
as well as two 16-bit registers for interfacing with memory (the
MAR--Memory Address Register--and MBR--Memory Buffer Register.) The
top 4 bits of the MAR is ignored, so the MIC-1 has a 12-bit address
space. Memory access is delayed by one cycle, during which the
appropriate flag must be asserted. If both flags are asserted, then
the external controller halts the machine.

The ALU's A side is either a register or the MBR. The shifter result
may be output to the MBR or any register. The MAR may be written from
the ALU's B side.

The top four words of memory (@litchar{4092}-@litchar{4095}) are wired
to a UART. The first two connect to the receiver and the second two
connect to the transmitter. The first of each holds an 8-bit character
to be outputed in the bottom 8 bits. The second of each holds a 4 bit
control flag in its lowest bits. The control bits are (from most to
least significant): On, Interrupt, Done, Busy. The control bits are
initialized to all zero. If the microprogram sets the On bit, then the
component is enabled and stabilizes. The receiver stabilizes to not
Done and Busy, while the transmitter stabilizes to Done and not
Busy. When the receiver receives a character, it switches to Done and
not Busy until the character is read by the CPU. When the program
writes a character to the transmit buffer while the transmitter is On,
then the transmitter switches to not Done and Busy, until the
transmission is finished. The Interrupt flag is currently ignored by
both components.

@section{@code{mic1} simulator}
@section-index{raco mic1}

@exec{raco mic1 @nonterm{option} ... @nonterm{microcode-path}
@nonterm{memory-image-path}} simulates the execution of the MIC-1.

@nonterm{microcode-path} must be a path to a file. If the extension is
@litchar{.prom}, then it must be in the @secref["mc-image"] format. If
the extension is @litchar{.mc}, then it must be in the @secref["mal"]
format and it will be compiled before loading.

@nonterm{memory-image-path} must be a path to a file. If the extension
is @litchar{.o}, then it must be in the @secref["mem-image"]
format. If the extension is @litchar{.s}, then it must be in the
@secref["mac-1"] format and it will be compiled before loading.

It accepts the following @nonterm{option}s:

@itemlist[

@item{@DFlag{ll} --- simulates at the NAND gate level via compilation to a
C program using @exec{cc}.}

@item{@DFlag{lli} --- simulates at the NAND gate level via an
interpreter.}

@item{@DFlag{hl} --- simulates at a high-level (default)}

@item{@DFlag{pc} @nonterm{pc-str} --- specifies the initial value of
the register 0, the Program Counter (default: 0)}

@item{@DFlag{sp} @nonterm{sp-str} --- specifies the initial value of
the register 2, the Stack Pointer (default: 1024)}

]

@subsection[#:tag "mc-image"]{Microcode Image}

A microcode image matches the grammar @nonterm{PROM}.

@BNF[(list @nonterm{PROM}
           @BNF-seq{@nonterm{Line} ...})
     (list @nonterm{Line}
           @BNF-seq[@nonterm{Entry} @litchar{\n}])
     (list @nonterm{Entry}
           @nonterm{MIR}
           @elem{@litchar{#} any sequence of character except @litchar{\n}})
     (list @nonterm{MIR}
           @BNF-seq[@nonterm{AMUX}
                    @nonterm{COND}
                    @nonterm{ALU}
                    @nonterm{SH}
                    @nonterm{MBR}
                    @nonterm{MAR}
                    @nonterm{RD}
                    @nonterm{WR}
                    @nonterm{ENC}
                    @nonterm{C}
                    @nonterm{B}
                    @nonterm{A}
                    @nonterm{ADDR}])
     (list @nonterm{AMUX}
           @elem{@litchar{0} --- ALU A side holds A register}
           @elem{@litchar{1} --- ALU A side holds MBR})
     (list @nonterm{COND}
           @elem{@litchar{00} --- Never jump}
           @elem{@litchar{01} --- Jump on negative ALU output}
           @elem{@litchar{10} --- Jump on zero ALU output}
           @elem{@litchar{11} --- Always jump})
     (list @nonterm{ALU}
           @elem{@litchar{00} --- A + B}
           @elem{@litchar{01} --- A & B}
           @elem{@litchar{10} --- A}
           @elem{@litchar{11} --- ! A})
     (list @nonterm{SH}
           @elem{@litchar{00} --- No shift}
           @elem{@litchar{01} --- Right shift}
           @elem{@litchar{10} --- Left shift})
     (list @nonterm{MBR}
           @elem{@litchar{0} --- Leave MBR unchanged}
           @elem{@litchar{1} --- Write shifter output to MBR})
     (list @nonterm{MAR}
           @elem{@litchar{0} --- Leave MAR unchanged}
           @elem{@litchar{1} --- Write ALU B side to MAR})
     (list @nonterm{RD}
           @elem{@litchar{0} --- Do not enable memory read}
           @elem{@litchar{1} --- Read from memory})
     (list @nonterm{WR}
           @elem{@litchar{0} --- Do not enable memory write}
           @elem{@litchar{1} --- Write to memory})
     (list @nonterm{ENC}
           @elem{@litchar{0} --- Do not save shifter output}
           @elem{@litchar{1} --- Write shifter output to @nonterm{C} register})
     (list @nonterm{C} @elem{4-bit register label})
     (list @nonterm{B} @elem{4-bit register label})
     (list @nonterm{A} @elem{4-bit register label})
     (list @nonterm{ADDR} @elem{8-bit microaddress})
     ]

In addition, a microcode image may only contain up to 256
@nonterm{mir} lines.

@subsection[#:tag "mem-image"]{Memory Image}

A memory image matches the grammar @nonterm{Image}.

@BNF[(list @nonterm{Image}
           @BNF-seq{@nonterm{Line} ...})
     (list @nonterm{Line}
           @BNF-seq[@nonterm{Entry} @litchar{\n}])
     (list @nonterm{Entry}
           @nonterm{Value}
           @elem{@litchar{#} any sequence of character except @litchar{\n}})
     (list @nonterm{Value}
           @elem{16-bit value written using the characters @litchar{0} and @litchar{1}})]

In addition, a memory image may only contain up to 4096
@nonterm{value} lines.

@section{@code{mcc} microcode compiler}
@section-index{raco mcc}

@exec{raco mcc @nonterm{microcode-path}} compiles @secref["mal"] into
the @secref["mc-image"] format.

@nonterm{microcode-path} must be a path to a file in the
@secref["mal"] format. @exec{raco mcc} replaces the extension of this
path with @litchar{.prom} and writes the corresponding
@secref["mc-image"].

@subsection[#:tag "mal"]{MAL microcode language}

While it is possible to directly write in the @secref["mc-image"]
format, it is extremely error-prone and tedious. MAL provides a
convenient way to write microprograms.

MAL supports block comments in between @litchar["{"] and
@litchar["}"]. Labels are sequences of any characters except
@litchar["\n (,:;)"].

A MAL program matches the following grammar @nonterm{Program}:

@BNF[(list @nonterm{Program}
           @BNF-seq{}
           @BNF-seq{@litchar{\n} @nonterm{Program}}
           @BNF-seq{@nonterm{Instruction} @litchar{\n} @nonterm{Program}})
     (list @nonterm{Instruction}
           @BNF-seq{}
           @BNF-seq{@nonterm{Component} @litchar[";"] @nonterm{Instruction}})]

@nonterm{Instruction}s are composed of multiple
@nonterm{Component}s. Each @nonterm{Component} determines some fields
of the @secref["mc-image"]. If two @nonterm{Component}s assign the
same field differently, then a compilation error is raised. The
following grammar specifies the various @nonterm{Component}s:

@BNF[(list @nonterm{Component}
           (elem @BNF-seq{@litchar{mar} @litchar{:=} @nonterm{BExpr}}
                 @elem{--- Writes the ALU B side to MAR})
           (elem @BNF-seq{@nonterm{Register} @litchar{:=} @nonterm{ShExpr}}
                 @elem{--- Writes the shifter output to given register})
           (elem @BNF-seq{@litchar{mbr} @litchar{:=} @nonterm{ShExpr}}
                 @elem{--- Writes the shifter output to MBR})
           (elem @BNF-seq{@litchar{alu} @litchar{:=} @nonterm{AluExpr}}
                 @elem{--- Sets the ALU output})
           (elem @BNF-seq{@litchar{if} @nonterm{Cond} @litchar{then} @litchar{goto} @nonterm{Label}}
                 @elem{--- Sets the COND flag and the ADDR value})
           (elem @BNF-seq{@litchar{goto} @nonterm{Label}}
                 @elem{--- Sets the COND flag to @litchar{11} and the ADDR value})
           (elem @litchar{rd}
                 @elem{--- Sets the RD flag})
           (elem @litchar{wr}
                 @elem{--- Sets the WR flag}))]

The remaining nonterminals are specified by the following grammar:

@BNF[(list @nonterm{Cond}
           @elem{@litchar{n} --- Jump on negative ALU output}
           @elem{@litchar{z} --- Jump on zero ALU output})
     (list @nonterm{ShExpr}
           @BNF-seq{@nonterm{AluExpr} @elem{--- Do not shift}}
           @BNF-seq{@litchar{lshift@"("} @nonterm{AluExpr} @litchar{@")"} @elem{--- Left shift}}
           @BNF-seq{@litchar{rshift@"("} @nonterm{AluExpr} @litchar{@")"} @elem{--- Right shift}})
     (list @nonterm{AluExpr}
           @BNF-seq{@nonterm{AExpr} @litchar{+} @nonterm{BExpr} @elem{--- Addition}}
           @BNF-seq{@litchar{band@"("} @nonterm{AExpr} @litchar{,} @nonterm{BExpr} @litchar{@")"} @elem{--- Bitwise And}}
           @BNF-seq{@nonterm{AExpr} @elem{--- Identity}}
           @BNF-seq{@litchar{inv@"("} @nonterm{AExpr} @litchar{@")"} @elem{--- Bitwise Negation}})
     (list @nonterm{AExpr}
           @nonterm{Register}
           @litchar{mbr})
     (list @nonterm{BExpr} @nonterm{Register})
     (list @nonterm{Register}
           @litchar{pc} @litchar{ac} @litchar{sp} @litchar{sp} @litchar{ir}
           @litchar{tir} @litchar{0} @litchar{1} @litchar{(-1)} @litchar{amask}
           @litchar{smask} @litchar{a} @litchar{b} @litchar{c} @litchar{d}
           @litchar{e} @litchar{f})]

If a MAL program produces an image greater than 256 instructions, then
no error is raised during compilation.

For examples see the
@link["https://github.com/jeapostrophe/mic1/tree/master/examples"]{Github
repository}, specifically: @exec{fib.mc} implements Fibonacci and
@exec{macro-v1.mc} implements an interpreter for compiled
@secref["mac-1"].

@section{@code{masm} macroassembler}
@section-index{raco masm}

@exec{raco masm @nonterm{asm-path}} compiles @secref["mac-1"]
into the @secref["mem-image"] format.

@nonterm{asm-path} must be a path to a file in the @secref["mac-1"]
format. @exec{raco masm} replaces the extension of this path with
@litchar{.o} and writes the corresponding @secref["mem-image"].

@subsection[#:tag "mac-1"]{MAC-1 macro-assembly}

The MAC-1 is a low-level virtual machine implemented by a MIC-1
microprogram. It exposes a single register (AC) to programmers and has
an internal state defined by two other registers (PC and SP).

The assembly language supports line comments starting with the
@litchar[";"] character. Whitespace is never significant. Literal
integers are supported in decimal format. Literal strings compile to
packed 16-bit words with early characters in least significant bits.

Labels are any alphanumeric character sequence starting with an
alphabetic character and ending in @litchar{:}. A label definition is
a label not in an argument position or immediately after a label
definition.

The character sequence @litchar{.LOC} followed by a literal
nonnegative integer skips the given amount of space in the resulting
image, filling it with @litchar{1111111111111111}.

The following instructions are recognized:

@(define X (make-string 12 #\x))
@(define Y (make-string  8 #\y))
@(define (instruction mnem arg pat desc semantics)
   (list @elem{@litchar{@mnem} @(if arg @nonterm{Arg} "")}
         @elem{@litchar{@|pat|@(or arg "")}}
         desc
         semantics))

@tabular[
 #:style 'boxed
 #:column-properties '(left center center right)
 #:row-properties '(bottom-border ())
 (list
  (list "Mnemonic" "Encoding" "Instruction" "Semantics")
  @instruction["LODD" X "0000" "Load Direct"          @elem{AC := Mem[X]}]
  @instruction["STOD" X "0001" "Store Direct"         @elem{Mem[X] := AC}]
  @instruction["ADDD" X "0010" "Add Direct"           @elem{AC := AC + Mem[X]}]
  @instruction["SUBD" X "0011" "Subtract Direct"      @elem{AC := AC - Mem[X]}]
  @instruction["JPOS" X "0100" "Jump on non-negative" @elem{If AC ≥ 0, PC := X}]
  @instruction["JZER" X "0101" "Jump on zero"         @elem{If AC = 0, PC := X}]
  @instruction["JUMP" X "0110" "Jump"                 @elem{PC := X}]
  @instruction["LOCO" X "0111" "Load Constant"        @elem{AC := X}]
  @instruction["LODL" X "1000" "Load Local"           @elem{AC := Mem[SP + X]}]
  @instruction["STOL" X "1001" "Store Local"          @elem{Mem[SP + X] := AC}]
  @instruction["ADDL" X "1010" "Add Local"            @elem{AC := AC + Mem[SP + X]}]
  @instruction["SUBL" X "1011" "Subtract Local"       @elem{AC := AC - Mem[SP + X]}]
  @instruction["JNEG" X "1100" "Jump on negative"     @elem{If AC < 0, PC := X}]
  @instruction["JNZE" X "1101" "Jump unless zero"     @elem{If AC ≠ 0, PC := X}]
  @instruction["CALL" X "1110" "Call"
               @elem{SP := SP - 1@";" Mem[SP] := PC@";" PC := X}]
  @instruction["PSHI" #f "1111000000000000" "Push Indirect"
               @elem{SP := SP - 1@";" Mem[SP] := Mem[AC]}]
  @instruction["POPI" #f "1111001000000000" "Pop Indirect"
               @elem{Mem[AC] := Mem[SP]@";" SP := SP + 1}]
  @instruction["PUSH" #f "1111010000000000" "Push"
               @elem{SP := SP - 1@";" Mem[SP] := AC}]
  @instruction[ "POP" #f "1111011000000000" "Pop"
                @elem{AC := Mem[SP]@";" SP := SP + 1}]
  @instruction["RETN" #f "1111100000000000" "Return"
               @elem{PC := Mem[SP]@";" SP := SP + 1}]
  @instruction["SWAP" #f "1111101000000000" "Swap AC & SP"
               @elem{AC :=: SP}]
  @instruction["INSP" Y "11111100" "Increment SP"
               @elem{SP := SP + Y}]
  @instruction["DESP" Y "11111110" "Decrement SP"
               @elem{SP := SP - Y}]
  @instruction["HALT" #f "1111111100000000" "Halt"
               @elem{Halt processor}])]

If a MAC-1 program produces an image greater than 4096 instructions,
then no error is raised during compilation.

For examples see the
@link["https://github.com/jeapostrophe/mic1/tree/master/examples"]{Github
repository}, specifically: @exec{fib.s} implements Fibonacci and
@exec{IO_str_and_echo.s} implements an echo program.

@section{HDL - General Purpose Hardware Description Language}

@(require (for-label mic1/hdl))
@defmodule[mic1/hdl]

XXX document HDL
