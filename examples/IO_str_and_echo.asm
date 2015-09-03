start:  lodd on:
        stod 4095
        call xbsywt:
        loco str1:
nextw:  pshi
        addd c1:
        stod pstr1:
        pop
        jzer crnl:
        stod 4094
        push
        subd c255:
        jneg crnl:
        call sb:
        insp 1
        push
        call xbsywt:
        pop
        stod 4094
        call xbsywt:
        lodd pstr1:
        jump nextw:
crnl:   lodd cr:
        stod 4094
        call xbsywt:
        lodd nl:
        stod 4094
        call xbsywt:
loop:   lodd on:                ; mic1 program to echo input
        stod 4093               ; using CSR memory locations
top:    lodd cnt:
        subd c1:
        stod cnt:
        jzer over:
	call rbsywt:
	lodd 4092
        stod 4094
	call xbsywt:
        jump top:
over:   lodd cr:
        stod 4094
        call xbsywt:
        lodd nl:
        stod 4094
	halt
xbsywt: lodd 4095
        subd mask:
        jneg xbsywt:
        retn
rbsywt: lodd 4093
        subd mask:
        jneg rbsywt:
        retn
sb:     loco 8
loop1:  jzer finish:
        subd c1:
        stod lpcnt:
        lodl 1
        jneg add1:
        addl 1
        stol 1
        lodd lpcnt:
        jump loop1:
add1:   addl 1
        addd c1:
        stol 1
        lodd lpcnt:
        jump loop1:
finish: lodl 1
        retn
lpcnt:  0
mask:   10
on:     8
nl:     10
cr:     13
cnt:    30
c1:     1
c255:   255
pstr1:  0
str1:   "THIS IS A TEST STRING1"
