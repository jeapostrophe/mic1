all: prom.dat adder.o help

help:
	@echo make adder.test

prom.dat: prom.mc
	../mcc/mcc < $^ > $@

%.test: prom.dat %.o
	../mic1/mic1 $^ 0 1024

%.o: %.asm
	../masm/masm < $^ > $@


