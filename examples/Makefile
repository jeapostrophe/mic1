help:
	@echo make adder.test

prom.dat: prom.mc
	../mcc/mcc < $^ > $@

%.test: prom.dat %.obj
	../mic1/mic1 $^ 0 1024

%.obj: %.asm
	../masm/masm < $^ > $@


