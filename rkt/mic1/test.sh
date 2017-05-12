#!/bin/bash

for MODE in --ll --lli --hl ; do
    racket main.rkt ${MODE} ../../examples/macro-v1.prom ../../examples/IO_str_and_echo.o < main.rkt
	racket main.rkt ${MODE} ../../examples/macro-v1.prom ../../examples/adder.o
	racket main.rkt ${MODE} ../../examples/fib.prom ../../examples/adder.o
done
