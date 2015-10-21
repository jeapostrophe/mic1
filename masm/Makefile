all: masm

masm: mic1symasm.o lex.yy.o
	cc -std=c99 -o masm mic1symasm.o lex.yy.o

mic1symasm.o:  mic1symasm.c
	cc -std=c99 -c -g mic1symasm.c

lex.yy.o:  lex.yy.c
	cc -std=c99 -c -g lex.yy.c

lex.yy.c: mic1symasm.ll
	flex mic1symasm.ll

clean:
	rm -f *.o masm lex.yy.c

test: masm test.asm test.o.expected
	./masm < test.asm > test.o
	diff -u test.o test.o.expected
