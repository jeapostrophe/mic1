START:
{Load the PC into the MAR and read it}
mar := pc; rd;
{Hold the read for two cycles, preemptively increment PC for next time}
pc := 1 + pc; rd;

{The instruction is in MBR, so save it and test the highest bit}
TEST_?:
ir := mbr; if n then goto TEST_1?;

TEST_0?:
tir := lshift(ir + ir); if n then goto TEST_01?;

TEST_00?:
tir := lshift(tir); if n then goto TEST_001?; 

TEST_000?:
alu := tir; if n then goto STOD;

LODD: {IR = 0000 ???? ???? ????}
{Treat the lower 12-bits of IR as an address and read it}
mar := ir; rd;
FINISH_READ:
{Wait for the read to complete}
rd;
{Assign the read value to AC and go back to interp start}
ac := mbr; goto START; 

STOD: {IR = 0001 ???? ???? ????}
{Treat the lower 12-bits of IR as an address, write AC to it}
mar := ir; mbr := ac; wr;
FINISH_WRITE:
{Hold the write and then go back to interp start}
wr; goto START;

TEST_001?:
alu := tir; if n then goto SUBD;

ADDD: {IR = 0010 ???? ???? ????}
{Treat the lower 12-bits of IR as an address and read it}
mar := ir; rd;
READ_AND_ADD:
{Hold the read}
rd; 
{Add the read value to AC and go back to interp start}
ac := ac + mbr; goto START; 

SUBD: {IR = 0011 ???? ???? ????}
{Treat the lower 12-bits of IR as an address and read it}
mar := ir; rd; 
SUBTRACT_A_FROM_AC:
{Hold the read and prepare to compute AC = AC + inv(MBR) + 1}
ac := 1 + ac; rd;
{Save the MBR and invert it}
a := inv(mbr);
{Assign AC = AC - MBR = AC + (inv(MBR) + 1) and go back to interp start}
ac := a + ac; goto START;

TEST_01?:
tir := lshift(tir); if n then goto TEST_011?; 

TEST_010?:
alu := tir; if n then goto JZER; 

JPOS: {IR = 0100 ???? ???? ????}
{Test if the AC is negative, if so go back to interp start and thus
 continue to the next line of the program}
alu := ac; if n then goto START;
{If it was negative, assign the PC to the bottom 12-bits and go back
 to interp start}
DIRECT_JUMP:
pc := band(ir, amask); goto START; 

JZER: {IR = 0101 ???? ???? ????}
{Test if the AC is zero, if so go up to DIRECT_JUMP, which will mask
 the bottom 12-bits and then continue, thus causing a jump}
alu := ac; if z then goto DIRECT_JUMP;
{Otherwise, go back to the interp start and continue program normally}
goto START; 

TEST_011?:
alu := tir; if n then goto LOCO; 

JUMP: {IR = 0110 ???? ???? ????}
{Assign the PC to bottom 12-bits and go to interp start}
pc := band(ir, amask); goto START; 

LOCO: {IR = 0111 ???? ???? ????}
{Assign the AC to bottom 12-bits and go to interp start}
ac := band(ir, amask); goto START;

TEST_1?:
tir := lshift(ir + ir); if n then goto TEST_11?; 

{IR = 10?? ???? ???? ????} {test next bit}
tir := lshift(tir); if n then goto TEST_101?; 

TEST_100?:
alu := tir; if n then goto STOL; 

LODL: {IR = 1000 ???? ???? ????}
{Compute the address of the local variable by treating the IR as an
 offset. Note that addresses ignore the top 4-bits, so the 1 in the
 high bit will not matter.}
a := sp + ir;
{Start a read to the address (remember that the mar must receive a
 register, so we cannot do the addition and assignment to mar at the
 same time) then go to FINISH_READ in the implementation of LODD which
 will complete the read and assign it to the AC.}
mar := a; rd; goto FINISH_READ; 

STOL: {IR = 1001 ???? ???? ????}
{Compute the address of the local variable}
a := sp + ir; 
{Start a write to the address then go to FINISH_WRITE in the
 implementation of STOD which will complete the write.}
mar := a; mbr := ac; wr; goto FINISH_WRITE; 

TEST_101?:
alu := tir; if n then goto SUBL; 

ADDL: {IR = 1010 ???? ???? ????}
{Compute the address of the local variable}
a := sp + ir; 
{Start a read and then go to READ_AND_ADD in the implementation of
 ADDD to finish}
mar := a; rd; goto READ_AND_ADD; 

SUBL: {IR = 1011 ???? ???? ????}
{Compute the address of the local variable}
a := sp + ir; 
{Start a read and then go to SUBTRACT_A_FROM_AC in the implementation
 of SUBD to finish}
mar := a; rd; goto SUBTRACT_A_FROM_AC; 

TEST_11?:
tir := lshift(tir); if n then goto TEST_111?; 

TEST_110?:
alu := tir; if n then goto JNZE; 

JNEG: {IR = 1100 ???? ???? ????}
{Test if the AC is negative, if so, goto JPOS implementation of direct
 jump}
alu := ac; if n then goto DIRECT_JUMP;
{Continue normally}
goto START; 

JNZE: {IR = 1101 ???? ???? ????}
{Test if the AC is zero, if so, continue normally}
alu := ac; if z then goto START;
{Do a direct jump (same code as DIRECT_JUMP, but needs to be here
 after the previous line)}
pc := band(ir, amask); goto START; 

TEST_111?:
tir := lshift(tir); if n then goto TEST_1111_?; 

CALL: {IR = 1110 ???? ???? ????}
{Make room on the stack to save the return address}
sp := sp + (-1);
{Write the return address (PC) to the stack}
mar := sp; mbr := pc; wr;
{Finish the write and do a direct jump}
pc := band(ir, amask); wr; goto START; 

TEST_1111_?:
tir := lshift(tir); if n then goto TEST_1111_1?; 

TEST_1111_0?:
tir := lshift(tir); if n then goto TEST_1111_01?; 

TEST_1111_00?:
alu := tir; if n then goto POPI; 

PSHI: {IR = 1111 000? ???? ????}
{Read from the AC}
mar := ac; rd;
{Finish the read and make space on the stack}
sp := sp + (-1); rd;
{Write to the stack, then goto STOD impl to finish write and continue}
mar := sp; wr; goto FINISH_WRITE; 

POPI: {IR = 1111 001? ???? ????}
{Read from the stack and remove space from the stack}
mar := sp; sp := sp + 1; rd;
{Finish the read}
rd;
{Write to the address in the AC and go to STOD impl to finish write
 and continue}
mar := ac; wr; goto FINISH_WRITE; 

TEST_1111_01?:
alu := tir; if n then goto POP; 

PUSH: {IR = 1111 010? ???? ????}
{Make space on stack}
sp := sp + (-1);
{Write AC to stack and go to STOD impl to finish}
mar := sp; mbr := ac; wr; goto FINISH_WRITE; 

POP: {IR = 1111 011? ???? ????}
{Read from stack and remove space}
mar := sp; sp := sp + 1; rd;
{Finish read}
rd;
{Save popped value into AC and continue}
ac := mbr; goto START; 

TEST_1111_1?:
tir := lshift(tir); if n then goto TEST_1111_11?; 

TEST_1111_10?:
alu := tir; if n then goto SWAP; 

RETN: {IR = 1111 100? ???? ????}
{Read the return address from the stack and decrease its size}
mar := sp; sp := sp + 1; rd;
{Finish the read}
rd;
{Jump to the location that was on the stack}
pc := mbr; goto START; 

SWAP: {IR = 1111 101? ???? ????}
{Save AC into A}
a := ac;
{Replace AC with SP}
ac := sp;
{Move saved AC into SP then continue}
sp := a; goto START; 

TEST_1111_11?:
alu := tir; if n then goto TEST_1111_111?; 

INSP: {IR = 1111 110? ???? ????}
{Mask away the bottom 8-bits of instruction}
a := band(ir, smask);
{Add them to the SP and continue}
FINISH_SP_MOD:
sp := sp + a; goto START; 

TEST_1111_111?:
tir := tir + tir; if n then goto HALT; 

DESP: {IR = 1111 1110 ???? ????}
{Mask away the bottom 8-bits of instruction}
a := band(ir, smask);
{Invert them, because we trying to subtract}
a := inv(a);
{Add one, so that A = -dSP, then go to INSP impl to do addition and
 continue}
a := a + 1; goto FINISH_SP_MOD; 

HALT: {IR = 1111 1111 ???? ????}
{Assert read and write, which signals interpreter to halt.}
rd; wr; goto START;
