START:
    LOCO 8
    STOD 4095
    STOD 4093
WRITE_LOOP:   
    LODD A_as_a_number:
    CALL Print_AC:
    ADDD One:
    STOD A_as_a_number:
    LODD How_many_letters:
    SUBD One:
    STOD How_many_letters:
    JZER DONE_ALPHA:
    JUMP WRITE_LOOP:
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

DONE_ALPHA:
    LOCO 10
    CALL Print_AC:
    JUMP DONE:
    
READ_LOOP:
    CALL Read_AC:
    CALL Print_AC:
    SUBD Q:
    JZER DONE:
    JUMP READ_LOOP:
    
DONE:   
    LOCO 10
    CALL Print_AC:
    HALT
A_as_a_number:  65
How_many_letters:   26
One:    1
Ten:    10
Q:  81
