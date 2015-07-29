/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : types.h			*/

#ifndef _TYPES_H
#define _TYPES_H

typedef enum { false, true } bool;

typedef enum 
{ 
	id=1, semi, assignop, lparen, rparen, plus, iconst, colon,
	reg, mar, alu, halt, nline, done, comma,
	lshift, rshift, rd, wr, n, z, inv, band,
	IF, THEN, GOTO
}tokens;


union attribute
{
	struct s_symnode *entry;
	int num;
};

typedef struct s_tokattr
{
	tokens token;
	union attribute attr;
}tokattr;

typedef enum
{
	r_pc = 0, r_ac, r_sp, r_ir, r_tir, r_0, r_1, r_n1, r_amask, r_smask,
	r_a, r_b, r_c, r_d, r_e, r_f, r_mbr
}enreg;

typedef struct s_symnode
{
	char* lexeme;
	tokens token;
	union
	{
		char *str;
		enreg r;
	} attr;
	struct s_symnode *next;
}	symnode;

#endif
