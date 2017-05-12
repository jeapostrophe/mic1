START:
{Write 0 to SP, D = SP+1 (next write destination)}
mar := sp; d := 1 + sp; wr;
{Finish write, F = 2}
f := 1 + 1; wr;
{Write 1 (B) to SP+1 (D)}
mbr := 1; b := 1; mar := d; wr;
{Finish write, E = 3}
e := 1 + f; wr;
LOOP:
{Assume A = Fib(n) and B = Fib(n+1), assign A = Fib(n+2)}
a := b + a;
{D = SP+2}
d := sp + f;
{Write Fib(n+2) (A) to SP+2 (D), halt if negative}
mar :=  d; mbr := a; wr; if n then goto DONE;
{Finish write, SP = SP + 3}
sp := e + sp; wr;
{Assume B = Fib(n+1) and A = Fib(n+2), assign B = Fib(n+3)}
b := a + b;
{Write Fib(n+3) (B) to SP+3 (SP), halt if negative}
mar := sp; mbr := b; wr; if n then goto DONE;
{SP = SP+2 (D), goto LOOP}
sp := d; goto LOOP; wr;
{Finish write, HALT}
DONE:
wr; rd; goto START;
