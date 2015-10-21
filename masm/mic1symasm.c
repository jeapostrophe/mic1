#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mic1symasm.h"
extern int yylex(void);
extern char* yytext;

#define HEADERS     1
#define NO_HEADERS  0

typedef struct nament {
  char           name[26];
  int            addr;
  struct nament *next;
} SYMTABENTRY;

char cstr_16[17];
void str_n(char n, short num) {
  for (int i=0; i<16; i++) {
    cstr_16[i] = '0';
  }
  cstr_16[16] = '\0';

  unsigned int mask = 1 << (n-1);
  for (int i=0; i<16; i++) {
    if (num & mask) {
      cstr_16[i] = '1';
    }
    mask >>= 1;
  }
}

void str_12(char *cstr) { str_n(12, atoi(cstr)); }
void str_16(char *cstr) { str_n(16, atoi(cstr)); }
void bstr_16(unsigned short bin_num) { str_n(16, bin_num); }

void generate_code(int);
void update_sym_table(char *);
void search_sym_table(char *);
void print_first_pass(int);
void append_table(void);
void dump_table(void);

FILE *p1;
int  label_pc = -1;
unsigned short pc = 0;
SYMTABENTRY *symtab = NULL;

void require_int(char n, const char *op) {
  int tok;
  if((tok=yylex()) != INTEG){
    fprintf(stderr, "Bad operand after %s is %s\n", op, yytext);
    exit(1);
  }
  str_n(n, atoi(yytext));
}

void emit_label_op(const char *op, const char *code) {
  int tok;
  switch(tok=yylex()){
  case INTEG:
    str_12(yytext);
    fprintf(p1, "%d  %s%.12s\n", pc, code, cstr_16);
    break;
  case LABEL:
    fprintf(p1, "%d  U%s000000000000    %s\n", pc, code, yytext);
    break;
  default:
    fprintf(stderr, "Bad operand after %s is %s on line %d\n", op, yytext, pc);
    exit(1);
  }
}

int main(int argc, char *argv[]) {
  int tok = 0, i = 0, dump_tab = 0, linum = 0, object_file = 0;
  unsigned short temp = 0;

  if (argc > 1 && (strcmp(argv[1], "-s") == 0)) {
    dump_tab = linum = 1;
  } else if (argc > 1 && (strcmp(argv[1], "-o") == 0)) {
    object_file = 1;
  }

  p1 = fopen("/tmp/passone", "w+");
  unlink("/tmp/passone");
  while( (tok=yylex()) ){
    switch(tok){
    case LODD: emit_label_op("LODL", "0000"); break;
    case STOD: emit_label_op("STOD", "0001"); break;
    case ADDD: emit_label_op("ADDD", "0010"); break;
    case SUBD: emit_label_op("SUBD", "0011"); break;
    case JPOS: emit_label_op("JPOS", "0100"); break;
    case JZER: emit_label_op("JZER", "0101"); break;
    case JUMP: emit_label_op("JUMP", "0110"); break;
    case JNEG: emit_label_op("JNEG", "1100"); break;
    case JNZE: emit_label_op("JNZE", "1101"); break;
    case CALL: emit_label_op("CALL", "1110"); break;

    case LOCO:
      switch(tok=yylex()){
      case INTEG:
        if(yytext[0] == '-'){
          fprintf(stderr,"Negative operand after LOCO is %s on line %d, must be positive !\n",yytext, pc);
          exit(1);
        }
        str_12(yytext);
        fprintf(p1,"%d  0111%.12s\n", pc, cstr_16);
        break;
      case LABEL:
        fprintf(p1,"%d  U0111000000000000    %s\n", pc, yytext);
        break;
      default:
        fprintf(stderr,"Bad operand after LOCO is %s on line %d\n",yytext, pc);
        exit(1);
      }
      break;

    case LODL:
      require_int(12, "LODL");
      fprintf(p1,"%d  1000%.12s\n", pc, cstr_16);
      break;

    case STOL:
      require_int(12, "STOL");
      fprintf(p1,"%d  1001%.12s\n", pc, cstr_16);
      break;

    case ADDL:
      require_int(12, "ADDL");
      fprintf(p1,"%d  1010%.12s\n",  pc, cstr_16);
      break;

    case SUBL:
      require_int(12, "SUBL");
      fprintf(p1,"%d  1011%.12s\n", pc, cstr_16);
      break;

    case PSHI:
      fprintf(p1,"%d  1111000000000000\n",pc);
      break;

    case POPI:
      fprintf(p1,"%d  1111001000000000\n",pc);
      break;

    case PUSH:
      fprintf(p1,"%d  1111010000000000\n",pc);
      break;

    case POP:
      fprintf(p1,"%d  1111011000000000\n",pc);
      break;

    case RETN:
      fprintf(p1,"%d  1111100000000000\n",pc);
      break;

    case SWAP:
      fprintf(p1,"%d  1111101000000000\n",pc);
      break;

    case INSP:
      require_int(8, "INSP");
      fprintf(p1,"%d  11111100%.8s\n", pc, cstr_16);
      break;

    case DESP:
      require_int(8, "DESP");
      fprintf(p1,"%d  11111110%.8s\n",  pc, cstr_16);
      break;

    case HALT:
      fprintf(p1,"%d  1111111111000000\n",pc);
      break;

    case INTEG:
      str_16(yytext);
      fprintf(p1,"%d  %.16s\n", pc, cstr_16);
      break;

    case LABEL:
      if (label_pc == pc) {   /* for < lbx: lby: >   */
        fprintf(p1,"%d  U0000000000000000    %s\n", pc, yytext);
        break;
      }
      search_sym_table(yytext);
      update_sym_table(yytext);
      label_pc = pc;
      pc--;
      break;

    case LOC:
      require_int(12, ".LOC");
      if((temp = ((unsigned short)atoi(yytext) )) < pc){
        fprintf(stderr,"Bad operand after .LOC is %s, TOO SMALL !\n",yytext);
        exit(1);
      }
      pc = temp - 1;
      break;

    case STR:
      i=1;
      do{
        if(*(yytext+i) == '\"'){
          bstr_16(0);
          fprintf(p1,"%d  %.16s\n", pc, cstr_16);
          break;
        }
        temp = (unsigned short)*(yytext+i++);
        if(*(yytext+i) != '\"'){
          temp = (temp | ((unsigned short)*(yytext+i) << 8));
        }
        bstr_16(temp);
        fprintf(p1,"%d  %.16s\n", pc, cstr_16);
      } while (*(yytext+i++) != '\"' && ++pc);
      break;

    case JUNK:
      fprintf(stderr,"Unrecognized token is %s\n",yytext);
      exit(26);

    default:
      fprintf(stderr,"Default case, unrecoverable error\n");
      exit(26);
    }
    pc++;
  }
  
  if (object_file) {
    print_first_pass(NO_HEADERS);
    append_table();
    return 0;
  }
  
  if(linum){
    print_first_pass(HEADERS);
  }
  
  generate_code(linum);
  
  if (dump_tab) {
    dump_table();
  }
 
  return 0;
}

void print_first_pass(int headers) {
  char inbuf[81];

  if (headers == HEADERS) {
    printf("\n  FIRST PASS \n");
    rewind(p1);
    while (fgets(inbuf, 80, p1) != NULL) {
      printf("   %s", inbuf);
    }
    printf("\n  SECOND PASS \n");
  } else {
    rewind(p1);
    while (fgets(inbuf, 80, p1) != NULL) {
      printf("   %s", inbuf);
    }
  }
}

void dump_table(void) {
  FILE *fd;
  fd = popen("sort", "w");
  printf("\n  SYMBOL TABLE \n");
  printf("***********************************************\n");
  for (struct nament *list = symtab; list != NULL; list = list->next) {
    fprintf(fd,"%-25s %4d\n",list->name, list->addr);
  }
  fclose(fd);
  wait(NULL);
  printf("***********************************************\n");
}

void append_table(void) {
  printf("  %d %s\n", 4096, "x");
  for (struct nament *list = symtab; list != NULL; list = list->next) {
    printf("    %-25s %4d\n",list->name, list->addr);
  }
}

int get_sym_val(char *symbol) {
  for (struct nament *list = symtab; list != NULL; list = list->next) {
    if (strcmp(list->name, symbol) == 0) {
      return(list->addr);
    }
  }
  return (-1);
}

void generate_code(int linum) {
  char linbuf[10] = {'0'};
  char instruction[18] = {'0'};
  int  line_number = 0;
  int  pc = 0, mask = 0, sym_val = 0, old_pc = 0, diff = 0;
  char symbol[26] = {'0'};

  rewind(p1);

  sprintf(linbuf,"%5d:  ", line_number);

  while (fscanf(p1,"%d %s", &pc, instruction) != EOF) {
    if ((diff = pc - old_pc ) > 1) {
      for (int j=1; j<diff; j++) {
        sprintf(linbuf,"%5d:  ", line_number++);
        printf("%s1111111111111111\n",(linum ? linbuf: "\0"));
      }
    }
    sprintf(linbuf,"%5d:  ", line_number++);
    old_pc = pc;

    if (instruction[0] == 'U') {
      fscanf(p1, "%s", symbol);
      if ((sym_val = get_sym_val(symbol)) == -1) {
        fprintf(stderr, "no symbol in symbol table: %s\n", symbol);
        exit(27);
      }

      for (int i=0; i<12; i++) {
        cstr_16[i] = '0';
      }
      cstr_16[12] = '\0';

      mask = 2048;
      for (int i=0; i<12; i++) {
        if (sym_val & mask) {
          cstr_16[i] = '1';
        }
        mask >>= 1;
      }
      for (int i=0; i<12; i++) {
        instruction[i+5] = cstr_16[i];
      }
      printf("%s%s\n", (linum ? linbuf: "\0"), &instruction[1]);
    } else {
      printf("%s%s\n", (linum ? linbuf: "\0"), instruction);
    }
  }
  fclose(p1);
}

void update_sym_table(char *symbol) {
  for (struct nament *list = symtab; list; list = list->next) {
    if ((strcmp(list->name, symbol)) == 0) {
      list->addr = pc;
      return;
    }
  }
  fprintf(stderr, "error from symbol table on line %d\n", pc);
  exit(27);
}

void search_sym_table(char *symbol) {
  for (struct nament *list = symtab; list; list = list->next) {
    if (strcmp(list->name, symbol) == 0) {      
      return;
    }
  }
  struct nament *element = malloc(sizeof (struct nament));
  strcpy(element->name, symbol);
  element->next = symtab;
  symtab = element;
}
