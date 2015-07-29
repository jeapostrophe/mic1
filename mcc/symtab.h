/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : symtab.h			*/

#ifndef _SYMTAB_H
#define _SYMTAB_H

#include "types.h"
#define TABLESIZE 47

symnode *lookup(char *name);
symnode *insert(char *name, tokens token);
void dumptab();
void inittab();

char *salloc(int size);
symnode *nalloc();

#endif
