all: build tests

build:
	raco make *rkt

tests:
	# XXX Make these tests check results and be more abstracted
	racket main.rkt --ll ../../examples/prom.dat ../../examples/IO_str_and_echo.o < main.rkt
	racket main.rkt --ll ../../examples/prom.dat ../../examples/adder.o
	racket main.rkt --ll ../../examples/fib.prom ../../examples/adder.o

	racket main.rkt --lli ../../examples/prom.dat ../../examples/IO_str_and_echo.o < main.rkt
	racket main.rkt --lli ../../examples/prom.dat ../../examples/adder.o
	racket main.rkt --lli../../examples/fib.prom ../../examples/adder.o

	racket main.rkt --hl ../../examples/prom.dat ../../examples/IO_str_and_echo.o < main.rkt
	racket main.rkt --hl ../../examples/prom.dat ../../examples/adder.o
	racket main.rkt --hl  ../../examples/adder.o
