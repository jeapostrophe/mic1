/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : symdrv.c			*/

#include <stdio.h>

#include "scanner.h"
#include "dbuffer.h"
#include "symtab.h"
#include "errors.h"
#include "types.h"

void main(int argc, char *argv[])
{
	tokattr pair;
	debuglevel |= symbug_m;
	if (argc != 2)
	{
		printf("%s: Usage = %s <filename>\n",argv[0],argv[0]);
		exit(1);
	}

	initbuf(argv[1]);
	inittab();

	do
		pair = gettoken();
	while (pair.token != done);
	dumptab();
}
