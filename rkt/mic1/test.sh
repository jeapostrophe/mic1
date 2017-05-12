#!/bin/bash

for MODE in --ll --lli --hl ; do
    racket mic1.rkt ${MODE} ../../examples/macro-v1.prom ../../examples/IO_str_and_echo.o < main.rkt
	racket mic1.rkt ${MODE} ../../examples/macro-v1.prom ../../examples/adder.o
	racket mic1.rkt ${MODE} ../../examples/fib.prom ../../examples/adder.o
done

