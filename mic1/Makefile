CFLAGS = -g --std=c99
OBJECTS = driver.o alu.o control.o clock.o cpu.o datapath.o memory.o shifter.o
HEADERS = mic1.h $(OBJECTS:.o=.h)

mic1 : $(OBJECTS)
	cc -o $@ $^

%.o: %.c $(HEADERS)
	cc -c $(CFLAGS) $(@:.o=.c)

clean:
	rm -f mic1 *.o
