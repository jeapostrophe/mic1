/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : symtab.c			*/

#include <string.h>

#include "mcc.h"
#include "scanner.h"
#include "symtab.h"
#include "errors.h"

static symnode *table[TABLESIZE];

/*----------------------------------------------------------------------*/

int hash( char* name )
{
  char* p = name;
  char current_char;
  unsigned hash_val = 0;
  unsigned upper_four;

  while ( ( current_char = *p++ ) != '\0' )
  {
    hash_val = ( hash_val << 4 ) + (unsigned) current_char;
    if ( (upper_four = hash_val & 0xf0000000) )
      hash_val ^= ( upper_four >> 24 ) | upper_four;
  }

  return hash_val % TABLESIZE;

}

/*----------------------------------------------------------------------*/

symnode *lookup(char *name)
{
	symnode *p = table[hash(name)];
	while (p!=NULL && strcmp(p->lexeme,name))
		p = p -> next;
	return(p);
}

/*----------------------------------------------------------------------*/

symnode *insert(char *name, tokens token)
{
	int h = hash(name);
	symnode *p = lookup(name);
	if (p==NULL)
	{
		p = table[h];
		table[h]        = nalloc();
		table[h]->next  = p;
		table[h]->lexeme= name;
		table[h]->token	= token;
	}
	else 
		printerr(symtaberr, reinsert,0);
	return(table[h]);
}

/*----------------------------------------------------------------------*/

void dumptab()
{
	int i;
	symnode *p;

	if (!(debuglevel & symbug_m))
		return;

	fprintf(stderr,"\nFull Symbol Table Dump\n");
	for (i=0; i < TABLESIZE; i++)
	{
		fprintf(stderr,"Bucket[%d]:\n",i);
		p = table[i];
		while (p !=NULL )
		{
			fprintf(stderr,"\ttoken = ");
			printtok(p->token);
			fprintf(stderr,", lex = %s\n", p->lexeme);
			fputc('\n', stderr);
			p = p-> next;
		}
	}
}

/*----------------------------------------------------------------------*/

void inittab()
{
	symnode *p;
	int i;

	for (i=0; i < TABLESIZE; i++)
		table[i] = NULL;
	insert(	strcpy(salloc(strlen("if")+1),"if"),IF);
	insert(	strcpy(salloc(strlen("then")+1),"then"),THEN);
	insert(	strcpy(salloc(strlen("goto")+1),"goto"),GOTO);
	insert(	strcpy(salloc(strlen("lshift")+1),"lshift"),lshift);
	insert(	strcpy(salloc(strlen("rshift")+1),"rshift"),rshift);
	insert(	strcpy(salloc(strlen("rd")+1),"rd"),rd);
	insert(	strcpy(salloc(strlen("wr")+1),"wr"),wr);
	insert(	strcpy(salloc(strlen("alu")+1),"alu"),alu);
	insert(	strcpy(salloc(strlen("mar")+1),"mar"),mar);
	insert(	strcpy(salloc(strlen("halt")+1),"halt"),halt);
	insert(	strcpy(salloc(strlen("n")+1),"n"),n);
	insert(	strcpy(salloc(strlen("z")+1),"z"),z);
	insert(	strcpy(salloc(strlen("inv")+1),"inv"),inv);
	insert(	strcpy(salloc(strlen("band")+1),"band"),band);

	p = insert(	strcpy(salloc(strlen("pc")+1),"pc"),reg);
	p->attr.r = r_pc;
	p = insert(	strcpy(salloc(strlen("ac")+1),"ac"),reg);
	p->attr.r = r_ac;
	p = insert(	strcpy(salloc(strlen("sp")+1),"sp"),reg);
	p->attr.r = r_sp;
	p = insert(	strcpy(salloc(strlen("ir")+1),"ir"),reg);
	p->attr.r = r_ir;
	p = insert(	strcpy(salloc(strlen("tir")+1),"tir"),reg);
	p->attr.r = r_tir;
	p = insert(	strcpy(salloc(strlen("amask")+1),"amask"),reg);
	p->attr.r = r_amask;
	p = insert(	strcpy(salloc(strlen("smask")+1),"smask"),reg);
	p->attr.r = r_smask;
	p = insert(	strcpy(salloc(strlen("a")+1),"a"),reg);
	p->attr.r = r_a;
	p = insert(	strcpy(salloc(strlen("b")+1),"b"),reg);
	p->attr.r = r_b;
	p = insert(	strcpy(salloc(strlen("c")+1),"c"),reg);
	p->attr.r = r_c;
	p = insert(	strcpy(salloc(strlen("d")+1),"d"),reg);
	p->attr.r = r_d;
	p = insert(	strcpy(salloc(strlen("e")+1),"e"),reg);
	p->attr.r = r_e;
	p = insert(	strcpy(salloc(strlen("f")+1),"f"),reg);
	p->attr.r = r_f;
	p = insert(	strcpy(salloc(strlen("mbr")+1),"mbr"),reg);
	p->attr.r = r_mbr;
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/* 
        _  _
       // //            /)              _/_
 __.  // // _____.     //  . . ____  _. /  o ________  _
(_/|_</_</_(_) (__    //__(_/_/ / <_(__<__<_(_) / / <_/_)_
                     />
                    </ 
*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

char *salloc(int size)
{
	static char *strtable;
	static int where=1024;

	if ((size + where) > 1024)
	{
		strtable = (char *) malloc(1024);
		where = 0;
	}
	strtable += size;
	where += size;
	return (strtable-size);
}

/*----------------------------------------------------------------------*/

symnode *nalloc()
{
	static symnode *head = NULL;
	static int where = 0;
	
	if (where == 0)
	{
		head = (symnode *) malloc(sizeof(symnode)*100);
		where = 100;
	}
	where--;
	return(head++);
}

/*----------------------------------------------------------------------*/
