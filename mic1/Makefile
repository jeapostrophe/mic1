mic1 : driver.o alu.o control.o clock.o cpu.o datapath.o memory.o shifter.o 
	gcc -o mic1 driver.o alu.o control.o clock.o cpu.o datapath.o memory.o shifter.o
driver.o: driver.c globals.h
	gcc -c -g driver.c 
alu.o: alu.c globals.h
	gcc -c -g alu.c
control.o: control.c globals.h
	gcc -c -g control.c
clock.o: clock.c 
	gcc -c -g clock.c
cpu.o: cpu.c globals.h
	gcc -c -g cpu.c
datapath.o: datapath.c  globals.h
	gcc -c -g datapath.c
memory.o: memory.c globals.h
	gcc -c -g  memory.c 
shifter.o: shifter.c globals.h
	gcc -c -g shifter.c
clean:
	rm mic1 *.o
