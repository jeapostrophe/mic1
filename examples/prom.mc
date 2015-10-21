{Load the PC into the MAR and read it}
0:mar := pc; rd;
{Hold the read for two cycles, preemptively increment PC for next time}
1:pc := 1 + pc; rd;

{The instruction is in MBR, so save it and test the highest bit}
2:ir := mbr; if n then goto 28;
{IR = 0??? ???? ???? ????} {test the next bit}
3:tir := lshift(ir + ir); if n then goto 19;
{IR = 00?? ???? ???? ????} {test the next bit}
4:tir := lshift(tir); if n then goto 11; 
{IR = 000? ???? ???? ????} {test the next bit}
5:alu := tir; if n then goto 9;

{IR = 0000 ???? ???? ????} {LODD}
{Treat the lower 12-bits of IR as an address and read it}
6:mar := ir; rd;
{Wait for the read to complete}
7:rd;
{Assign the read value to AC and go back to interp start}
8:ac := mbr; goto 0; 

{IR = 0001 ???? ???? ????} {STOD}
{Treat the lower 12-bits of IR as an address, write AC to it}
9:mar := ir; mbr := ac; wr;
{Hold the write and then go back to interp start}
10:wr; goto 0;

{IR = 001? ???? ???? ????} {test the next bit}
11:alu := tir; if n then goto 15;

{IR = 0010 ???? ???? ????} {ADDD}
{Treat the lower 12-bits of IR as an address and read it}
12:mar := ir; rd;
{Hold the read}
13:rd; 
{Add the read value to AC and go back to interp start}
14:ac := ac + mbr; goto 0; 

{IR = 0011 ???? ???? ????} {SUBD}
{Treat the lower 12-bits of IR as an address and read it}
15:mar := ir; rd; 
{Hold the read and prepare to compute AC = AC + inv(MBR) + 1}
16:ac := 1 + ac; rd;
{Save the MBR and invert it}
17:a := inv(mbr);
{Assign AC = AC - MBR = AC + (inv(MBR) + 1) and go back to interp start}
18:ac := a + ac; goto 0;

{IR = 01?? ???? ???? ????} {test next bit}
19:tir := lshift(tir); if n then goto 25; 

{IR = 010? ???? ???? ????} {test next bit}
20:alu := tir; if n then goto 23; 

{IR = 0100 ???? ???? ????} {JPOS}
{Test if the AC is negative, if so go back to interp start and thus
 continue to the next line of the program}
21:alu := ac; if n then goto 0;
{If it was negative, assign the PC to the bottom 12-bits and go back
 to interp start}
22:pc := band(ir, amask); goto 0; 

{IR = 0101 ???? ???? ????} {JZER}
{Test if the AC is zero, if so go up to 22, which will mask the bottom
 12-bits and then continue, thus causing a jump}
23:alu := ac; if z then goto 22;
{Otherwise, go back to the interp start and continue program normally}
24:goto 0; 

{IR = 011? ???? ???? ????} {test next bit}
25:alu := tir; if n then goto 27; 

{IR = 0110 ???? ???? ????} {JMP}
{Assign the PC to bottom 12-bits and go to interp start}
26:pc := band(ir, amask); goto 0; 

{IR = 0111 ???? ???? ????} {LOCO}
{Assign the AC to bottom 12-bits and go to interp start}
27:ac := band(ir, amask); goto 0;

{IR = 1??? ???? ???? ????} {test next bit}
28:tir := lshift(ir + ir); if n then goto 40; 

{IR = 10?? ???? ???? ????} {test next bit}
29:tir := lshift(tir); if n then goto 35; 

{IR = 100? ???? ???? ????} {test next bit}
30:alu := tir; if n then goto 33; 

{IR = 1000 ???? ???? ????} {LODL}
{Compute the address of the local variable by treating the IR as an
 offset. Note that addresses ignore the top 4-bits, so the 1 in the
 high bit will not matter.}
31:a := sp + ir;
{Start a read to the address (remember that the mar must receive a
 register, so we cannot do the addition and assignment to mar at the
 same time) then go to 7 in the implementation of LODD which will
 complete the read and assign it to the AC.}
32:mar := a; rd; goto 7; 

{IR = 1001 ???? ???? ????} {STOL}
{Compute the address of the local variable}
33:a := sp + ir; 
{Start a write to the address then go to 10 in the implementation of
 STOD which will complete the write.}
34:mar := a; mbr := ac; wr; goto 10; 

{IR = 101? ???? ???? ????} {test next bit}
35:alu := tir; if n then goto 38; 

{IR = 1010 ???? ???? ????} {ADDL}
{Compute the address of the local variable}
36:a := sp + ir; 
{Start a read and then go to 13 in the implementation of ADDD to
 finish}
37:mar := a; rd; goto 13; 

{IR = 1011 ???? ???? ????} {SUBL}
{Compute the address of the local variable}
38:a := sp + ir; 
{Start a read and then go to 16 in the implementation of SUBD to
 finish}
39:mar := a; rd; goto 16; 

{IR = 11?? ???? ???? ????} {test next bit}
40:tir := lshift(tir); if n then goto 46; 

{IR = 110? ???? ???? ????} {test next bit}
41:alu := tir; if n then goto 44; 

{IR = 1100 ???? ???? ????} {JNEG}
{Test if the AC is negative, if so, goto JPOS implementation of direct
 jump}
42:alu := ac; if n then goto 22;
{Continue normally}
43:goto 0; 

{IR = 1101 ???? ???? ????} {JNZE}
{Test if the AC is zero, if so, continue normally}
44:alu := ac; if z then goto 0;
{Do a direct jump (same code as 22, but needs to be here after the
 previous line)}
45:pc := band(ir, amask); goto 0; 

{IR = 111? ???? ???? ????} {test next bit}
46:tir := lshift(tir); if n then goto 50; 

{IR = 1110 ???? ???? ????} {CALL}
{Make room on the stack to save the return address}
47:sp := sp + (-1);
{Write the return address (PC) to the stack}
48:mar := sp; mbr := pc; wr;
{Finish the write and do a direct jump}
49:pc := band(ir, amask); wr; goto 0; 

{IR = 1111 ???? ???? ????} {test next bit}
50:tir := lshift(tir); if n then goto 65; 

{IR = 1111 0??? ???? ????} {test next bit}
51:tir := lshift(tir); if n then goto 59; 

{IR = 1111 00?? ???? ????} {test next bit}
52:alu := tir; if n then goto 56; 

{IR = 1111 000? ???? ????} {PSHI}
{Read from the AC}
53:mar := ac; rd;
{Finish the read and make space on the stack}
54:sp := sp + (-1); rd;
{Write to the stack, then goto STOD impl to finish write and continue}
55:mar := sp; wr; goto 10; 

{IR = 1111 001? ???? ????} {POPI}
{Read from the stack and remove space from the stack}
56:mar := sp; sp := sp + 1; rd;
{Finish the read}
57:rd;
{Write to the address in the AC and go to STOD impl to finish write
 and continue}
58:mar := ac; wr; goto 10; 

{IR = 1111 01?? ???? ????} {test next bit}
59:alu := tir; if n then goto 62; 

{IR = 1111 010? ???? ????} {PUSH}
{Make space on stack}
60:sp := sp + (-1);
{Write AC to stack and go to STOD impl to finish}
61:mar := sp; mbr := ac; wr; goto 10; 

{IR = 1111 011? ???? ????} {POP}
{Read from stack and remove space}
62:mar := sp; sp := sp + 1; rd;
{Finish read}
63:rd;
{Save popped value into AC and continue}
64:ac := mbr; goto 0; 

{IR = 1111 1??? ???? ????} {test next bit}
65:tir := lshift(tir); if n then goto 73; 

{IR = 1111 10?? ???? ????} {test next bit}
66:alu := tir; if n then goto 70; 

{IR = 1111 100? ???? ????} {RETN}
{Read the return address from the stack and decrease its size}
67:mar := sp; sp := sp + 1; rd;
{Finish the read}
68:rd;
{Jump to the location that was on the stack}
69:pc := mbr; goto 0; 

{IR = 1111 101? ???? ????} {SWAP}
{Save AC into A}
70:a := ac;
{Replace AC with SP}
71:ac := sp;
{Move saved AC into SP then continue}
72:sp := a; goto 0; 

{IR = 1111 11?? ???? ????} {test next bit}
73:alu := tir; if n then goto 76; 

{IR = 1111 110? ???? ????} {INSP}
{Mask away the bottom 8-bits of instruction}
74:a := band(ir, smask);
{Add them to the SP and continue}
75:sp := sp + a; goto 0; 

{IR = 1111 111? ???? ????} {test next bit}
76:tir := tir + tir; if n then goto 80; 

{IR = 1111 1110 ???? ????} {DESP}
{Mask away the bottom 8-bits of instruction}
77:a := band(ir, smask);
{Invert them, because we trying to subtract}
78:a := inv(a);
{Add one, so that A = -dSP, then go to INSP impl to do addition and
 continue}
79:a := a + 1; goto 75; 

{IR = 1111 1111 ???? ????} {HALT}
{Assert read and write, which signals interpreter to halt.}
80:rd; wr; 
