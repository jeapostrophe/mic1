all: masm

masm: masm.o lex.yy.o
	cc -std=c99 -o masm masm.o lex.yy.o

masm.o:  masm.c masm.h
	cc -std=c99 -c -g masm.c

lex.yy.o:  lex.yy.c masm.h
	cc -std=c99 -c -g lex.yy.c

lex.yy.c: masm.ll masm.h
	flex masm.ll

clean:
	rm -f *.o masm lex.yy.c

test: masm test.s test.o.expected
	./masm < test.s > test.o
	diff -u test.o test.o.expected
