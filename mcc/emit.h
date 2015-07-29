/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : emit.h				*/

#ifndef _EMIT_H
#define _EMIT_H

#include "types.h"

void initemit();
void unlinkoutfile();
void dumpword();

void genread(bool set);
void genwrite(bool set);
void genmar(bool set);
void genenc(bool set);
void genmbr(bool set);
void genamux(bool set);
void genareg(enreg dreg);
void genbreg(enreg dreg);
void gencreg(enreg dreg);
void gencond(int c);
void genshift(int f);
void genalu(int f);
void genaddr(int addr);
void genhalt();
void genabreg(enreg dreg);
void itob(int *a, int size, int num);
bool isequalb(int *a, int *b, int size);

#endif
