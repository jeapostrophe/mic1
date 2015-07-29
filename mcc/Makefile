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


# dependencies from makedepend
dbuffer.o : dbuffer.c /usr/include/string.h \
  /usr/include/sys/file.h /usr/include/sys/types.h \
  /usr/include/unistd.h mcc.h /usr/include/stdio.h /usr/include/stdlib.h \
  dbuffer.h symtab.h types.h errors.h scanner.h 
emit.o : emit.c /usr/include/ctype.h /usr/include/unistd.h mcc.h \
  /usr/include/stdio.h /usr/include/stdlib.h emit.h types.h errors.h \
  scanner.h 
errors.o : errors.c /usr/include/stdio.h mcc.h /usr/include/stdlib.h \
  errors.h scanner.h types.h emit.h dbuffer.h 
mcc.o : mcc.c mcc.h /usr/include/stdio.h /usr/include/stdlib.h symtab.h \
  types.h dbuffer.h scanner.h parser.h errors.h emit.h 
parser.o : parser.c mcc.h /usr/include/stdio.h /usr/include/stdlib.h \
  parser.h scanner.h types.h errors.h emit.h 
	gcc -Wall -g -c -fno-builtin parser.c
scandrv.o : scandrv.c /usr/include/stdio.h /usr/include/stdlib.h scanner.h \
  types.h dbuffer.h errors.h 
scanner.o : scanner.c /usr/include/ctype.h /usr/include/string.h \
  /usr/include/stdio.h /usr/include/stdlib.h mcc.h scanner.h types.h dbuffer.h \
  errors.h symtab.h 
symdrv.o : symdrv.c /usr/include/stdio.h scanner.h types.h dbuffer.h \
  symtab.h errors.h 
symtab.o : symtab.c /usr/include/string.h mcc.h /usr/include/stdio.h \
  /usr/include/stdlib.h scanner.h types.h symtab.h errors.h 
