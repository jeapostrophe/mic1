/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : dbuffer.h			*/

#ifndef _DBUFFER_H
#define _DBUFFER_H

extern int linenum;
extern int eof;
void initbuf(char *);
char getbc();
void ungetbc();
void moveupline();
char *getlinex();
char *getlex();
int getcurpos();
void alignptrs();

#endif
