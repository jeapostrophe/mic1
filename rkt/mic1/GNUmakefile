all: build tests

build:
	raco make *rkt

tests:
	racket mic1.rkt --hl ../../examples/macro-v1.mc ../../examples/adder.s
	bash test.sh
