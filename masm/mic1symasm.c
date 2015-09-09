
/*** #include "lex.yy.c" ***/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mic1symasm.h"

char cstr_6[7];
char cstr_8[9];
char cstr_12[13];
char cstr_16[17];
char binstr_16[17];

int  label_pc = -1;

void  str_6();
void  str_8();
void  str_12();
void  str_16();
void  bstr_16();
void  generate_code();
void  update_sym_table();
void  search_sym_table();
void  dump_table(void);

unsigned short pc = 0;
FILE  *p1;

struct nament{
	char   name[26];
	int    addr;
	struct nament *next;
};

struct nament *symtab = (struct nament *)0;

extern int yylex(void);
extern char * yytext;

int  main(int argc, char *argv[])
{
        int tok, i, dump_tab=0, linum=0;
	unsigned short temp;
/*        printf("COMMAND RECOGNIZER, ENTER COMMAND AND CR\n\n");    */

       if(argc > 1 && (strcmp(argv[1], "-s") == 0)) dump_tab = linum = 1;

	p1 = fopen("/tmp/passone", "w+");
	unlink("/tmp/passone");
        while(tok=yylex()){
        switch(tok){
          case 1:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0000%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0000000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after LODD is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 2:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0001%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0001000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after STOD is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 3:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0010%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0010000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after ADDD is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 4:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0011%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0011000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after SUBD is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 5:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0100%s\n",  pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0100000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after JPOS is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 6:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0101%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0101000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after JZER is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 7:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  0110%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0110000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after JUMP is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 8:  switch(tok=yylex()){
		    case INTEG: 
		     if(yytext[0] == '-'){
		       fprintf(stderr,"Negative operand after LOCO is %s on line %d, must be positive !\n",yytext, pc);
                     exit(1);
		     }
		     str_12(yytext);
                     fprintf(p1,"%d  0111%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U0111000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after LOCO is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

/***
          case 8:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after LOCO is %s\n",yytext);
                       exit(1);
                   }
		   str_12(yytext);
                   fprintf(p1,"%d  0111%s\n",  pc, cstr_12);
                   break;
***/

          case 9:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after LODL is %s\n",yytext);
                       exit(1);
                   }
		   str_12(yytext);
                   fprintf(p1,"%d  1000%s\n", pc, cstr_12);
                   break;

          case 10:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after STOL is %s\n",yytext);
                       exit(1);
                   }
		   str_12(yytext);
                   fprintf(p1,"%d  1001%s\n", pc, cstr_12);
                   break;

          case 11:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after ADDL is %s\n",yytext);
                       exit(1);
                   }
		   str_12(yytext);
                   fprintf(p1,"%d  1010%s\n",  pc, cstr_12);
                   break;

          case 12:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after SUBL is %s\n",yytext);
                       exit(1);
                   }
		   str_12(yytext);
                   fprintf(p1,"%d  1011%s\n", pc, cstr_12);
                   break;

          case 13: switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  1100%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U1100000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after JNEG is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

           case 14: switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  1101%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U1101000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after JNZE is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 15:  switch(tok=yylex()){
		    case INTEG: 
		     str_12(yytext);
                     fprintf(p1,"%d  1110%s\n", pc, cstr_12);
                     break;
		    case LABEL:
		     fprintf(p1,"%d  U1110000000000000    %s\n", pc, yytext);
                     break;
		    default:
                     fprintf(stderr,"Bad operand after CALL is %s on line %d\n",yytext, pc);
                     exit(1);
		   }
		   break;

          case 16: fprintf(p1,"%d  1111000000000000\n",pc);
                   break;

          case 17: fprintf(p1,"%d  1111001000000000\n",pc);
                   break;

          case 18: fprintf(p1,"%d  1111010000000000\n",pc);
                   break;

          case 19: fprintf(p1,"%d  1111011000000000\n",pc);
                   break;

          case 20: fprintf(p1,"%d  1111100000000000\n",pc);
                   break;

          case 21: fprintf(p1,"%d  1111101000000000\n",pc);
                   break;

          case 22: if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after INSP is %s\n",yytext);
                       exit(1);
                   }
		   str_8(yytext);
                   fprintf(p1,"%d  11111100%s\n", pc, cstr_8);
                   break;

          case 23: if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after DESP is %s\n",yytext);
                       exit(1);
                   }
		   str_8(yytext);
                   fprintf(p1,"%d  11111110%s\n",  pc, cstr_8);
                   break;

          case 24: fprintf(p1,"%d  1111111100000000\n",pc);
                   break;


          case 25:  str_16(yytext);
		    fprintf(p1,"%d  %s\n", pc, cstr_16);
 /*                   fprintf(stderr,"Misplaced integer is %s\n",yytext);   */
 /*                   exit(25);     */
                    break;

	  case 27:  if (label_pc == pc){	/* for < lbx: lby: >   */
			fprintf(p1,"%d  U0000000000000000    %s\n", pc, yytext);
		        break;
		    }
		    search_sym_table(yytext);
		    update_sym_table(yytext);
		    label_pc = pc;
		    pc--;
		    break;


	  case 28:  if((tok=yylex()) != INTEG){
                       fprintf(stderr,"Bad operand after .LOC is %s\n",yytext);
                       exit(1);
                   }
		    if((temp = ((unsigned short)atoi(yytext) )) < pc){
		       fprintf(stderr,"Bad operand after .LOC is %s, TOO SMALL !\n",yytext);
                       exit(1);
                   }

		   pc = temp - 1;
                   break;

	  case 29:  i=1;
		    do{
			if(*(yytext+i) == '\"'){
			  bstr_16(0);
			  fprintf(p1,"%d  %s\n", pc, binstr_16);
			  break;
			}
			temp = (unsigned short)*(yytext+i++);
			if(*(yytext+i) != '\"'){
			temp = (temp | ((unsigned short)*(yytext+i) << 8));
			}
		        bstr_16(temp);
                        fprintf(p1,"%d  %s\n", pc, binstr_16);
		    }while(*(yytext+i++) != '\"' && ++pc);
		    break;


          case 26:  fprintf(stderr,"Unrecognized token is %s\n",yytext);
                    exit(26);

          default:  fprintf(stderr,"Default case, unrecoverable error\n");
                    exit(26); 
   }
   pc++;
}
/**** 
   fclose(p1); 
****/
   generate_code(linum);

   if(dump_tab)dump_table();

/*    printf("IF I GET HERE THE PROGRAM TERMINATED PROPERLY\n\n");    */
}
void dump_table(void){
	FILE *fd;
	struct nament *list;
	fd = popen("sort +0 -1 -f", "w");
	printf("***********************************************\n");
        for(list = symtab; list != (struct nament *)0; list = list->next){
		fprintf(fd,"%-25s %4d\n",list->name, list->addr);
	}
	fclose(fd);
	wait(NULL);
	printf("***********************************************\n");
}

int get_sym_val(symbol)
	char *symbol;
{
	int i,j;
	struct nament *element, *list;

	for(list = symtab; list != (struct nament *)0; list = list->next){
		if(strcmp(list->name, symbol) == 0){
		   return(list->addr);
		}
	}
	return(-1);
}


void generate_code(int linum){
/****   FILE  *p1;   ****/
	char linbuf[10];
	char instruction[18];
	int  line_number;
	int  pc, mask, sym_val,i, j, old_pc, diff;
	char symbol[26];

	line_number = old_pc = 0;
	rewind(p1);

	sprintf(linbuf,"%5d:  ", line_number);

	while(fscanf(p1,"%d %s", &pc, instruction) != EOF){
	if((diff = pc - old_pc ) > 1){
	  for(j=1; j<diff; j++){
		sprintf(linbuf,"%5d:  ", line_number++);
		printf("%s1111111111111111\n",(linum ? linbuf: "\0"));
	  }
	}
	sprintf(linbuf,"%5d:  ", line_number++);
	old_pc = pc;

	 if(instruction[0] == 'U'){
	   fscanf(p1, "%s", symbol);
	   if((sym_val = get_sym_val(symbol)) == -1){
		fprintf(stderr, "no symbol in symbol table: %s\n", symbol);
		exit(27);
	   }
	   	
           for(i=0; i<12; i++){
	     cstr_12[i] = '0';
	   }
	   cstr_12[12] = '\0';
         
	   mask = 2048;
           for(i=0; i<12; i++){
	      if(sym_val & mask)
		  cstr_12[i] = '1';
	      mask >>= 1;
	   }
	   for(i=0; i<12; i++){
		instruction[i+5] = cstr_12[i];
	   }
	   printf("%s%s\n",(linum ? linbuf: "\0"),&instruction[1]);
	 }else
	   printf("%s%s\n",(linum ? linbuf: "\0"),instruction);
	}
	fclose(p1);
}

		
	

void update_sym_table(symbol)
	char *symbol;
{
	int i,j;
	struct nament *element, *list;

	for(list = symtab; list; list = list->next){
		if((strcmp(list->name, symbol)) == 0){
		    list->addr = pc;
		    return;
		}
	}
	fprintf(stderr, "error from symbol table on line %d\n", pc);
	exit(27);
}

void search_sym_table(symbol)
	char *symbol;
{
	int i,j;
	struct nament *element, *list;

	for(list = symtab; list; list = list->next){
		if(strcmp(list->name, symbol) == 0)return;
	}
	element = malloc(sizeof (struct nament));
	strcpy(element->name, symbol);
	element->next = symtab;
	symtab = element;
}

void    str_6(char *cstr)
{
        unsigned short str_val;
        int i,j,k,mask;

        str_val = (unsigned short)atoi(cstr);

        for(i=0; i<6; i++){
           cstr_6[i] = '0';
        }
        cstr_6[6] = '\0';

        j=0;
        mask = 32;
        for(i=0; i<6; i++){
           if(str_val & mask)
                cstr_6[i] = '1';
           mask >>= 1;
        }

}

void    str_8(cstr)
	char *cstr;
{
	unsigned short str_val;
	int i,j,k,mask;

	str_val = (unsigned short)atoi(cstr);
	
        for(i=0; i<8; i++){
	   cstr_8[i] = '0';
	}
	cstr_8[8] = '\0';
         
	j=0;
	mask = 128;
        for(i=0; i<8; i++){
	   if(str_val & mask)
		cstr_8[i] = '1';
	   mask >>= 1;
	}

}

void    str_12(cstr)
	char *cstr;
{
	unsigned short str_val;
	int i,j,k,mask;

	str_val = (unsigned short)atoi(cstr);
	
        for(i=0; i<12; i++){
	   cstr_12[i] = '0';
	}
	cstr_12[12] = '\0';
         
	j=0;
	mask = 2048;
        for(i=0; i<12; i++){
	   if(str_val & mask)
		cstr_12[i] = '1';
	   mask >>= 1;
	}

}

void    str_16(cstr)
	char *cstr;
{
	short str_val;
	int i,j,k,mask;

	str_val = (short)atoi(cstr);
	
        for(i=0; i<16; i++){
	   cstr_16[i] = '0';
	}
	cstr_16[16] = '\0';
         
	mask = (1024 * 32);
        for(i=0; i<16; i++){
	   if(str_val & mask)
		cstr_16[i] = '1';
	   mask >>= 1;
	}

}

void	bstr_16(bin_num)
	unsigned short bin_num;
{
        short str_val;
        int i,j,k,mask;

        str_val = bin_num;

        for(i=0; i<16; i++){
           binstr_16[i] = '0';
        }
        binstr_16[16] = '\0';

        mask = (1024 * 32);
        for(i=0; i<16; i++){
           if(str_val & mask)
                binstr_16[i] = '1';
           mask >>= 1;
        }

}


/***
void    str_16(cstr)
	char *cstr;
{
	unsigned short str_val;
	int i,j,k,mask;

	str_val = (unsigned short)atoi(cstr);
	
        for(i=0; i<16; i++){
	   cstr_16[i] = '0';
	}
	cstr_16[16] = '\0';
         
	mask = (1024 * 32);
        for(i=0; i<16; i++){
	   if(str_val & mask)
		cstr_16[i] = '1';
	   mask >>= 1;
	}

}
***/
