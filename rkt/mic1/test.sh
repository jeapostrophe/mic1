#!/bin/bash

for MODE in --ll --lli --hl ; do
    racket main.rkt ${MODE} ../../examples/prom.dat ../../examples/IO_str_and_echo.o < main.rkt
	racket main.rkt ${MODE} ../../examples/prom.dat ../../examples/adder.o
	racket main.rkt ${MODE} ../../examples/fib.prom ../../examples/adder.o
done
