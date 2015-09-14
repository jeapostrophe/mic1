/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : scanner.c			*/

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "mcc.h"
#include "scanner.h"
#include "dbuffer.h"
#include "errors.h"
#include "symtab.h"

/*----------------------------------------------------------------------*/

void getcomment()
{
	char ch;

	do
	{
		ch = getbc();
		if ( ch == '\n')
		{
			moveupline();
			if ((debuglevel & scanbug_m) && !eof)
				fprintf(stderr,"\n%d:%s",linenum,getlinex());
		}
	} while (ch != '}' && ch != EOF);
	if (ch == EOF)
	{
		eof = 1;
		printerr(syntaxerr,unexpeof,0, NULL);
	}
	alignptrs();
}

/*----------------------------------------------------------------------*/

void getassignorcolontok(tokattr *pair)
{
	char ch = getbc();
	if (ch == '=')
		pair->token = assignop;
	else
	{
		pair->token = colon;
		ungetbc();
	}
}

/*----------------------------------------------------------------------*/

void getidtok(tokattr *pair)
{
	char ch;
	do
	{
		ch = getbc();
	} while (isalnum(ch));
	if (ch == EOF)
	{
		eof = 1;
		printerr(syntaxerr,unexpeof,0, NULL);
	}
	ungetbc();
	pair->token = id;
	pair->attr.entry = lookup(getlex());
	if (pair->attr.entry == NULL)
	{
      printerr(parseerr, unknownid, 0, NULL);
		pair->token = 0;
	}
	else
		pair->token = pair->attr.entry->token;
}

/*----------------------------------------------------------------------*/

void getnumtok(tokattr *pair)
{
	char ch;

	do
	{
		ch = getbc();
	} while (isdigit(ch) || ch == '-');

	if (ch == EOF)
	{
		eof = 1;
		printerr(syntaxerr,unexpeof,0, NULL);
	}
	ungetbc();	/* unget ch used for test in if */
	pair->token = iconst;
	pair->attr.num = atoi(getlex());
}

/*----------------------------------------------------------------------*/

void printpair(tokattr pair)
{
	fprintf(stderr,"(");

	printtok(pair.token);

	switch(pair.token)
	{
		case id:			fprintf(stderr,",%s",pair.attr.entry->lexeme);
							break;
		case reg:			fprintf(stderr, ",%s",pair.attr.entry->lexeme);
		case done:			fprintf(stderr,")\n");		return;
		case iconst:		fprintf(stderr,",%d",pair.attr.num);	break;
		default:			break;
	}
	fprintf(stderr,")");
}

/*----------------------------------------------------------------------*/

void printtok(tokens token)
{
	switch(token)
	{
		case id:			fprintf(stderr,"id");			break;
		case halt:			fprintf(stderr, "halt");		break;
		case reg:			fprintf(stderr, "reg");			break;
		case mar:			fprintf(stderr, "mar");			break;
		case alu:			fprintf(stderr, "alu");			break;
		case band:			fprintf(stderr, "band");		break;
		case inv:			fprintf(stderr, "inv");			break;
		case comma:			fprintf(stderr,",");			break;
		case semi:			fprintf(stderr,";");			break;
		case colon:			fprintf(stderr,":");			break;
		case assignop:		fprintf(stderr,":=");			break;
		case lparen:		fprintf(stderr,"(");			break;
		case rparen:		fprintf(stderr,")");			break;
		case plus:			fprintf(stderr,"+");			break;
		case iconst:		fprintf(stderr,"integer constant");	 	break;
		case nline:			fprintf(stderr,"newline");		break;
		case done:			fprintf(stderr,"End of File");	break;
		case IF:			fprintf(stderr,"if");			break;
		case THEN:			fprintf(stderr,"then");			break;
		case GOTO:			fprintf(stderr,"goto");			break;
		case wr:			fprintf(stderr, "wr");			break;
		case rd:			fprintf(stderr, "rd");			break;
		case lshift:		fprintf(stderr, "lshift");		break;
		case rshift:		fprintf(stderr, "rshift");		break;
		case z:				fprintf(stderr, "z");			break;
		case n:				fprintf(stderr, "n");			break;
	}
}

/*----------------------------------------------------------------------*/

tokattr gettoken()
{
	char ch;
	tokattr pair;
	static bool gotdone = false;
	static bool newline = false;
	bool recursed = false;
	if (gotdone)
	{
		if (debuglevel & parsebug_m)
			fprintf(stderr,"Error: trying to read passed end of file\n");
		pair.token=done;
		return(pair);
	}
	else
		advanced = true;

	do
	{
		ch = getbc();		
	} while (ch != EOF && (ch == ' ' || ch == '\t'));

	if (newline)
	{
		moveupline();
		if ((debuglevel & scanbug_m) && !eof)
				fprintf(stderr,"\n%d:%s",linenum,getlinex());
		newline = false;
	}

	ungetbc(); alignptrs(); getbc();
	switch (ch)
	{
		case EOF:	pair.token = done;			break;
		case '\n':	pair.token = nline;			newline = true;	break;
		case '(':	pair.token = lparen;		break;
		case ')':	pair.token = rparen;		break;
		case ',':	pair.token = comma;			break;
		case ';':	pair.token = semi;			break;
		case '+':	pair.token = plus;			break;
		case ':':	getassignorcolontok(&pair);	break;
		case '{':	getcomment();				pair = gettoken();
					recursed = true;			break;
		default:	if (isalpha(ch))
						getidtok(&pair);	
					else if (isdigit(ch) || ch == '-')
						getnumtok(&pair);
					else 
					{
                      printerr(syntaxerr,norecog,0, NULL);
						pair = gettoken();
						recursed = true;
					}
	}
	if (!recursed && (debuglevel & scanbug_m))
			printpair(pair);
	if (pair.token == done)
		gotdone=true;
	return(pair);
}

/*----------------------------------------------------------------------*/
