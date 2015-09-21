#.SUFFIXES : .h .c .o .cy .hy

COMP = gcc

CFLGS = -Wall -g

.c.o:
	$(COMP) $(CFLGS) -c $<

all: mcc mcv

mcv  :	mcv.o
	$(COMP) $(CFLGS) -o mcv mcv.o

mcc  :	mcc.o scanner.o errors.o dbuffer.o symtab.o parser.o emit.o
	$(COMP) $(CFLGS) -o mcc mcc.o scanner.o errors.o dbuffer.o symtab.o parser.o emit.o

symdrv  :	scanner.o errors.o dbuffer.o symtab.o symdrv.o
	$(COMP) $(CFLGS) -o symdrv scanner.o errors.o dbuffer.o symtab.o symdrv.o

scandrv :	scanner.o errors.o dbuffer.o symtab.o scandrv.o
	$(COMP) $(CFLGS) -o scandrv scanner.o errors.o dbuffer.o symtab.o scandrv.o

clean   : 
	rm -f *.o core mcc mcv
