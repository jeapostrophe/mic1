all: check-prom adder.o help

help:
	@echo make adder.test

check-prom: macro-v1.prom
	diff -u $^ macro-v1.prom.expected

%.prom: %.mc ../mcc/mcc
	../mcc/mcc < $< > $@

%.test: macro-v1.prom %.o
	../mic1/mic1 $^ 0 1024

%.o: %.s
	../masm/masm < $^ > $@


