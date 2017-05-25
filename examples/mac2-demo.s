INIT:   
    LOCO 8
    STOD 4093
    STOD 4095
START:
    LOCO 87
    CALL PrintInBinary:
    LOCO 10
    CALL Print_AC:
    
    LOCO 87
    BNEG
    CALL PrintInBinary:
    LOCO 10
    CALL Print_AC:

    LOCO 87
    ANDC 7
    CALL PrintInBinary:
    LOCO 10
    CALL Print_AC:

    LOCO 87
    MULC 4
    CALL PrintInBinary:
    LOCO 10
    CALL Print_AC:

    JUMP DONE:
DONE:   
    HALT

One:    1
Ten:    10
Print_AC:
    PUSH
    STOD 4094
TX_KeepWaiting:
    LODD 4095
    SUBD Ten:
    JNEG TX_KeepWaiting:
    POP
    RETN
Read_AC:
    LODD 4093
    SUBD Ten:
    JNEG Read_AC:
    LODD 4092
    RETN

PIB_Count:  9999
PrintInBinary:
    PUSH
    LOCO 16
    STOD PIB_Count:
    POP
PIB_LOOP:
    PUSH
    JNEG PIB_One:
    LOCO 48
    CALL Print_AC:
    JUMP PIB_Next:
PIB_One:
    LOCO 49
    CALL Print_AC:
PIB_Next:
    LODD PIB_Count:
    SUBD One:
    JZER PIB_Done:
    STOD PIB_Count:
    POP
    LSHIFT
    JUMP PIB_LOOP:
PIB_Done:
    POP
    RETN
