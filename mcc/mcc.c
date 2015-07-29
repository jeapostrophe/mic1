/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : mc.c				*/

#include "mcc.h"
#include "symtab.h"
#include "dbuffer.h"
#include "scanner.h"
#include "parser.h"
#include "errors.h"
#include "emit.h"

tokattr current;

int main(int argc, char *argv[])
{
	debuglevel |= 0;
	
	fprintf(stderr, "Microcode Compiler - Version 1.0 - Richard Boccuzzi\n");

	if (argc <  2)
	{
	   initbuf(NULL);
	}else{
	   initbuf(argv[1]);
	}
	
	inittab();
	initemit();
	current = gettoken();	

	program();
	return 0;
}
