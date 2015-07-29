#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef enum {false, true} bool;

char *getreg(char *s)
{
	typedef enum
	{
		r_pc, r_ac, r_sp,
		r_ir, r_tir,
		r_z, r_o, r_no,
		r_amask, r_smask, 
		r_a, r_b, r_c, r_d, r_e, r_f
	} reg;
	char *sr = (char *) malloc(10);
	reg r = 8 * (s[0] - '0') + 4 * (s[1]-'0') + 2 * (s[2]-'0') + (s[3]-'0');
	
	switch (r)
	{
		case r_pc:		sprintf(sr,"pc");		break;
		case r_ac:		sprintf(sr,"ac");		break;
		case r_sp:		sprintf(sr,"sp");		break;
		case r_ir:		sprintf(sr,"ir");		break;
		case r_tir:		sprintf(sr,"tir");		break;
		case r_z:		sprintf(sr,"0");		break;
		case r_o:		sprintf(sr,"1");		break;
		case r_no:		sprintf(sr,"(-1)");		break;
		case r_amask:	sprintf(sr,"amask");	break;
		case r_smask:	sprintf(sr,"smask");	break;
		case r_a:		sprintf(sr,"a");		break;
		case r_b:		sprintf(sr,"b");		break;
		case r_c:		sprintf(sr,"c");		break;
		case r_d:		sprintf(sr,"d");		break;
		case r_e:		sprintf(sr,"e");		break;
		case r_f:		sprintf(sr,"f");		break;
	}
	return(sr);
}

char *getops(char *line)
{
	bool amux = line[0] - '0';
	int alu =  2 * (line[3] - '0') + 1 * (line[4] - '0');
	int sh  =  2 * (line[5] - '0') + 1 * (line[6] - '0');
	int a = 20, b = 16;
	char sareg[10], sshiftl[10], sshiftr = '\0';
	char *s = (char *) malloc(20);

	if (amux)
		sprintf(sareg, "mbr");
	else
		strcpy(sareg, getreg(line+a)); 

	switch(sh)
	{
		case 0:	strcpy(sshiftl, "");	 	break;
		case 1:	strcpy(sshiftl, "rshift(");	sshiftr = ')';	break;
		case 2: strcpy(sshiftl, "lshift(");	sshiftr = ')';	break;
	}

	switch(alu)
	{
		case 0:	sprintf(s, "%s%s + %s%c", sshiftl, getreg(line+b), sareg, sshiftr);
				break;
		case 1:	sprintf(s, "%sband(%s, %s)%c", sshiftl, getreg(line+b), sareg, sshiftr);
				break;
		case 2:	sprintf(s, "%s%s%c", sshiftl, sareg, sshiftr);
				break;
		case 3:	sprintf(s, "%sinv(%s)%c", sshiftl, sareg, sshiftr);
				break;
	}
	return(s);
}

void printline(char *line)
{
	int addr, cond;
	bool mbr, mar, rd, wr, enc;
	int b = 16, c = 12;

	cond =  2 	* (line[1] - '0') + 1 	* (line[2] - '0');
	mbr		= line[7]  - '0';
	mar		= line[8]  - '0';
	rd		= line[9]  - '0';
	wr		= line[10] - '0';
	enc		= line[11] - '0';
	addr = 
		+ 128 	* (line[24] - '0')
		+ 64 	* (line[25] - '0')
		+ 32 	* (line[26] - '0')
		+ 16 	* (line[27] - '0')
		+ 8 	* (line[28] - '0')
		+ 4 	* (line[29] - '0')
		+ 2 	* (line[30] - '0') 
		+ 1 	* (line[31] - '0');

	if (mar)
		printf("mar := %s; ", getreg(line + b));

	if (mbr)
		printf("mbr := %s; ", getops(line));
	
	if (enc)
		printf("%s := %s; ",getreg(line + c), getops(line));

	if (rd)
		printf("rd; ");

	if (wr)
		printf("wr; ");

	if (!mbr && !enc && cond != 0 && cond != 3 )
		printf("alu := %s; ",getops(line));

	if (cond)
		switch (cond)
		{
			case 1:	printf("if n then goto %d; ", addr);	break;
			case 2:	printf("if z then goto %d; ", addr);	break;
			case 3:	printf("goto %d; ", addr);				break;
		}
	printf("\n");

}

int main(int argc, char *argv[])
{
	int linenum = 0;
	char line[33];
	FILE *fp;

	fprintf(stderr, "Microcode Viewer - Version 1.0 - Richard Boccuzzi\n");

	if (argc < 2)
	{
	   fp = stdin;
	}else{

	  if (!(fp = fopen(argv[1],"r")))
	  {
		fprintf(stderr, "Error, could not access %s!\n",argv[1]);
		exit(1);
	  }
	}

	while (fscanf(fp,"%32c",line) > 0) 
	{
		if (line[0] == '0' || line[0] == '1') 
		{
			printf("%d:",linenum++);
			printline(line);
		}
		while (getc(fp) != '\n');
	}
	return 0;
}

