/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : errors.c			*/

#include <stdio.h>

#include "mcc.h"
#include "errors.h"
#include "emit.h"
#include "dbuffer.h"
#include "scanner.h"

enum debuglevels debuglevel = 0;
bool advanced = true;
bool panicmode = false;

/*------------------------------------------------------------------*/

void printerr(enum classes class, unsigned short type, tokens t)
{
	static int oldlinenum = 0;
	if (!panicmode)
	{
		unlinkoutfile();
		panicmode = true;
	}
	if (linenum != oldlinenum)
		fprintf(stderr, "\n%4d:%s",oldlinenum = linenum, getlinex());
	else if (!advanced)
		return;
	advanced=false;
		
	fprintf(stderr,"%*c^\n",getcurpos()+4,' ');
	switch(class)
	{
		case syntaxerr: 
				fprintf(stderr, "Syntax error: ");
				switch (type)
				{
					case unexpeof:	
							fprintf(stderr, "Unexpected end of file\n");
							exit(1);
							break;
 					case norecog:	
							fprintf(stderr, "Unrecognizable character\n");
							break;
				}
				break;
		case symtaberr:
				fprintf(stderr, "Symbol Table Error: ");
				switch(type)
				{
					case reinsert:
							fprintf(stderr, "Insert of id already in table\n");
							break;
					case reserved:
							fprintf(stderr, "Reinsertion of reserved word\n");
							break;
				}
				break;
		case parseerr:
				switch(type)
				{
					case nomatch:
							fprintf(stderr, "Expecting ");
							printtok(t);
							fprintf(stderr, "\n");
							break;
					case nomar:
							fprintf(stderr, "Improper use of mar\n");
							break;
					case expectreg:
							fprintf(stderr, "Expecting register\n");
							break;
					case unknownid:
							fprintf(stderr, "Unknown identifier\n");
							break;
					case expectnz:
							fprintf(stderr, "Expecting n or z\n");
							break;
				}
				break;
		case semanticerr:
				switch(type)
				{
					case reset:
							fprintf(stderr, "Error, reseting %s to new value\n", (char *) t);
							break;
					case overusembr:
							fprintf(stderr, "Error, overusing mbr\n");
							break;
					case wrongmbr:
							fprintf(stderr, "Improper use of mbr(wrong bus)\n");
							break;
				}
				break;
	}
}

/*------------------------------------------------------------------*/

void panic()
{
	while (current.token != nline && current.token != semi)
		current = gettoken();
}

/*------------------------------------------------------------------*/
