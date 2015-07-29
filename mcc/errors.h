/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : errors.h			*/

#ifndef _ERRORS_H
#define _ERRORS_H

#include "scanner.h"

enum debuglevels 
{
	scanbug_m = 0x1, 
	symbug_m = 0x2,
	parsebug_m = 0x4,
};


enum classes
{
	syntaxerr, symtaberr,parseerr, semanticerr
};

enum syntaxtype { unexpeof, norecog };
enum symtabtype { reinsert, reserved };
enum parseerrtype { nomatch, nomar, expectreg, unknownid, expectnz };
enum semantictype { reset, overusembr, wrongmbr };

extern enum debuglevels debuglevel;
extern bool advanced;
extern bool panicmode;

extern void printerr(enum classes, unsigned short types, tokens t);
extern void panic();

#endif
