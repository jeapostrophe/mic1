/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : parser.c			*/

#include <unistd.h>
#include <string.h>

#include "mcc.h"
#include "parser.h"
#include "scanner.h"
#include "types.h"
#include "errors.h"
#include "emit.h"

void expr();

int level = 0;

/*----------------------------------------------------------------------*/

tokattr *match(tokens t)
{
	static tokattr oldpair;
	oldpair = current;
	if (t == current.token)
	{
		current = gettoken();
		return(&oldpair);
	}
	else
	{	
      printerr(parseerr, nomatch, t, NULL);
		panic();
		return(NULL);
	}
}

/*----------------------------------------------------------------------*/

void trace(char *name)
{
	if (debuglevel & parsebug_m)
		fprintf(stderr,"%*c<%s>\n",3*level,' ',name);
}

/*----------------------------------------------------------------------*/

enreg matchreg()
{
	tokattr *t;
	bool paren = false;
	enreg ret = r_pc;
	level++;
	trace("matchreg");
	switch(current.token)
	{
		case reg:		t = match(reg);
						ret = t->attr.entry->attr.r;
						break;
		case lparen:	match(lparen);
						paren = true;
		case iconst:	t = match(iconst);
						if (t->attr.num == 1)
							ret = r_1;
						else if (t->attr.num == -1)
							ret = r_n1;
						else if (t->attr.num == 0)
							ret = r_0;
						else
                          printerr(parseerr, expectreg, 0, NULL);
						if (paren)
							match(rparen);
						break;
    case mar:		printerr(parseerr, nomar, 0, NULL);
						break;
    default:		printerr(parseerr, expectreg, 0, NULL);
						break;
	}
	level--;
	return ret;
}

/*----------------------------------------------------------------------*/

bool choice()
{
	bool isjumpn = false;
	level++;
	trace("choice");
	if (current.token == n)
	{
		match(n);
		isjumpn = true;
	}
	else if (current.token == z)
	{
		match(z);		
		isjumpn = false;
	}
	else
	{	
      printerr(parseerr, expectnz, 0, NULL);
		if (current.token != THEN)
			match(current.token);
	}
	level--;
	return isjumpn;
}

/*----------------------------------------------------------------------*/

void aluexp_p(enreg olddreg)
{
	enreg dreg;
	level++;
	trace("aluexp_p");
	if (current.token == plus)
	{
		match(plus);
		genalu(0);
		dreg = matchreg();
		if (dreg == r_mbr && olddreg == r_mbr)
          printerr(semanticerr, overusembr, 0, NULL);
		else if (dreg == olddreg)
		{
			genareg(dreg);
			genbreg(dreg);
		}
		else
		{
			genabreg(dreg);
			genabreg(olddreg);
		}
	}
	else
	{
		genareg(olddreg);
		genalu(2);
	}
	level--;
}

/*----------------------------------------------------------------------*/

void aluexp()
{
	enreg dreg, olddreg;
	level++;
	trace("aluexp");
	switch(current.token)
	{
		case inv:	match(inv);
					genalu(3);
					match(lparen);
					dreg = matchreg();
					genareg(dreg);
					match(rparen);
					break;
		case band:	match(band);
					genalu(1);
					match(lparen);
					olddreg = matchreg();
					match(comma);
					dreg = matchreg();
					if (dreg == r_mbr && olddreg == r_mbr)
                      printerr(semanticerr, overusembr, 0, NULL);
					else if (dreg == olddreg)
					{
						genareg(dreg);
						genbreg(dreg);
					}
					else
					{
						genabreg(dreg);
						genabreg(olddreg);
					}
					match(rparen);
					break;
		case lparen:
		case iconst:
		case reg:	dreg = matchreg();
					aluexp_p(dreg);
					break;
    case mar:	printerr(parseerr, nomar, 0, NULL);
					break;
    default:	printerr(parseerr, unknownid, 0, NULL);
					break;
	}
	level--;
}

/*----------------------------------------------------------------------*/

void expr()
{
	level++;
	trace("exp");
	switch(current.token)
	{
		case lshift:match(lshift);
					genshift(2);
					match(lparen);
					aluexp();
					match(rparen);
					break;
		case rshift:match(rshift);
					genshift(1);
					match(lparen);
					aluexp();
					match(rparen);
					break;
		default:	genshift(0);
					aluexp();
					break;
	}
	level--;
}

/*----------------------------------------------------------------------*/

void statement()
{
	enreg dreg;
	bool isjumpn;
	tokattr *pair;
	level++;
	trace("statement");
	switch(current.token)
	{
		case rd:		match(rd);
						genread(true);
						break;	
		case wr:		match(wr);
						genwrite(true);
						break;	
		case alu:		match(alu);
						genenc(false);
						match(assignop);
						expr();
						break;	
		case mar:		match(mar);
						genmar(true);
						match(assignop);
						dreg = matchreg();
						genbreg(dreg);
						break;	
		case iconst:
		case reg:		dreg = matchreg();
						gencreg(dreg);
						if (dreg != r_mbr) genenc(true);
						match(assignop);
						expr();
						break;	
		case IF:		match(IF);
						isjumpn = choice();
						gencond(isjumpn ? 1 : 2 );
						match(THEN);
						match(GOTO);
						pair = match(id);
						genaddr(pair->attr.id);
						break;	
		case GOTO:		match(GOTO);
						gencond(3);
						pair = match(id);
						genaddr(pair->attr.id);
						break;	
		case halt:		match(halt);
						genhalt();
						break;	
		default:		break;	
	}
	level--;
}

/*----------------------------------------------------------------------*/

void another_statement()
{
	level++;
	trace("another_statement");
	if (current.token != nline)
	{
		match(semi);
		statement();
		another_statement();
	}
	level--;
}

/*----------------------------------------------------------------------*/

typedef struct SymbolTableEntry {
  char *name;
  int value;
  struct SymbolTableEntry *next;
} SymbolTableEntry_t;

SymbolTableEntry_t *SymbolTable = NULL;

int LookupSymbol(char *name) {
  SymbolTableEntry_t *cur = SymbolTable;
  while (cur != NULL) {
    if (strcmp(name, cur->name) == 0) {
      return cur->value;
    } else {
      cur = cur->next;
    }
  }
  return -1;
}

void consume_nline() {
  while (current.token == nline) {
    match(nline);
  }
}

void program_firstpass()
{
	tokattr *t;
    char *label;
    int line;
    SymbolTableEntry_t *new_entry;
    
	level++;
	trace("program");
    line = 0;
	while (current.token != done) {
      consume_nline();
      while (current.token == id) {
        t = match(id);
        label = t->attr.id;
        match(colon);

        new_entry = malloc(sizeof(SymbolTableEntry_t));
        new_entry->name = label;
        new_entry->value = line;
        new_entry->next = SymbolTable;    
        SymbolTable = new_entry;

        consume_nline();
      }
      statement();
      another_statement();
      match(nline);
      dumpword();
      line++;
	}
	level--;
}

void program_secondpass(FILE *firstpass) {
  rewind(firstpass);

  char instruction[32] = {'0'};
  char label[1024] = {'0'};
  int label_ref = 0;
  while (fscanf(firstpass, "%s", instruction) != EOF) {
    if (instruction[0] == 'U') {
      fscanf(firstpass, "%s", instruction);
      copy_instruction_to_word(instruction);
      fscanf(firstpass, "%s", label);
      label_ref = LookupSymbol(label);
      if (label_ref == -1) {
        fprintf(stderr, "unresolved label reference: %s\n", label);
        exit(1);
      }
      
      genrealaddr(label_ref);
    } else {
      copy_instruction_to_word(instruction);
    }
    dumpword();
  }
}

void program() {
  FILE *firstpass = fopen("/tmp/mcc_passone", "w+");
  unlink("/tmp/mcc_passone");
  emit_change_outfile(firstpass);
  program_firstpass();
  emit_change_outfile(stdout);
  program_secondpass(firstpass);
}

/*----------------------------------------------------------------------*/
