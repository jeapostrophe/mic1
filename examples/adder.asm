start:  lodd daddr:  ;load AC with data address
        push         ;push AC to stack (2nd arg)
        lodd dcnt:   ;load AC with data count
        push         ;push AC to stack (1st arg)
        call adder:  ;push return address on stack
        stod rslt:   ;store AC (has sum) to rslt: location 
        halt         ;enter debugger
daddr:  data:        ;location holds data array address
data:   25           ;first of 5 data values
        50
        75
        100
        125          ;last of 5 data values
dcnt:   5            ;location holds data array element count 
rslt:   0            ;location for the sum to be stored
        .LOC 20      ;forces adder routine to start at location 20
adder:  lodl 1       ;get 1st arg from stack into AC (data count)
        stod mycnt:  ;store count at location mycnt:
        lodl 2       ;get 2nd arg from stack into AC (data addr)
        pshi         ;push indirect first datum to stack
        addd myc1:   ;add 1 (value at myc1:) to addr in AC
        stod myptr:  ;store new addr to location myptr:
loop:   lodd mycnt:  ;load AC with value at mycnt: (data count)
        subd myc1:   ;subtract 1 (value at myc1:) from AC
        jzer done:   ;if new data count is 0 go to location done: 
        stod mycnt:  ;if more data to add, store new data count
        lodd myptr:  ;load AC with addr of next datum
        pshi         ;push indirect next datum to stack
        addd myc1:   ;add 1 (value at myc1:) to addr in AC
        stod myptr:  ;store new addr to location myptr:
        pop          ;pop top of stack into AC (new datum)
        addl 0       ;add new top of stack location to AC
        insp 1       ;move stack pointer down one place
        push         ;push new sum in AC onto stack
        jump  loop:  ;jump to location loop:
done:   pop          ;come here when all data added, sum in AC
        retn         ;return to caller
        halt         ;should never get here (safety halt)
mycnt:  0            ;location for running count
myptr:  0            ;location for running data pointer
myc1:   1            ;location of a constant value of 1
