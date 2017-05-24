LOOP:
    ;; Ai = M[A + Ni]
    LODD A:                     ; AC <- A
    ADDD Ni:
    SWAP                        ; SP <- A, AC = SP
    STOD SPi:                   ; SPi <- SP
    LODL 0                      ; AC <- A[i]
    STOD Ai:                    ; Ai <- A[i]

    ;; Bi = M[B + Ni]
    LODD B:
    ADDD Ni:
    SWAP                        
    LODL 0                      

    ;; Ci = Ai + Bi
    ADDD Ai:
    STOD Ci:

    ;; PUSH Ci
    LODD SPi:                   
    SWAP
    LODD Ci:
    PUSH

    ;; Ni++
    LODD Ni:
    ADDD One:
    STOD Ni:
    SUBD N:

    JNZE LOOP:
    
    HALT
One:    1
SPi:    0
Ai: 0
Bi: 0
Ci: 0
Ni: 0
A: ADATA:
B: BDATA:
N:  10
ADATA:
    0 2 4 6 8 10 12 14 16 18
BDATA:
    1 3 5 7 9 11 13 15 17 19
