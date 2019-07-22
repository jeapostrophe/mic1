#define _POSIX_C_SOURCE 2
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

void generate_code();
void update_sym_table(char *);
void search_sym_table(char *);
void print_first_pass();
void append_table(void);
void dump_table(void);

FILE *p1;
unsigned short pc = 0;

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

void str_16(char *cstr) { str_n(16, atoi(cstr)); }
void bstr_16(unsigned short bin_num) { str_n(16, bin_num); }

#ifndef __MASM_AS_LIBRARY__

#include "masm.h"
extern int yylex(void);
extern char* yytext;

void emit_label_op(char n, const char *op, const char *code) {
  int tok;
  switch(tok=yylex()){
  case INTEG:
    str_n(12, atoi(yytext));
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

void emit_fixed_op(char n, const char *op, const char *code) {
  fprintf(p1, "%d  %s", pc, code);
  for ( int i = strlen(code); i < 16; i++ ) {
    fprintf(p1, "0");
  }
  fprintf(p1, "\n");
}

void require_int(char n, const char *op) {
  int tok;
  if((tok=yylex()) != INTEG){
    fprintf(stderr, "Bad operand after %s is %s\n", op, yytext);
    exit(1);
  }
  str_n(n, atoi(yytext));
}

void emit_int_op(char n, const char *op, const char* code) {
  require_int(n, op);
  fprintf(p1,"%d  %s%.*s\n", pc, code, n, cstr_16);
}

void generate_first_pass() {
  int tok = 0, i = 0;
  unsigned short temp = 0;
  int  label_pc = -1;

  while( (tok=yylex()) ) {
    switch(tok){
    case LODD: emit_label_op(12, "LODD", "0000"); break;
    case STOD: emit_label_op(12, "STOD", "0001"); break;
    case ADDD: emit_label_op(12, "ADDD", "0010"); break;
    case SUBD: emit_label_op(12, "SUBD", "0011"); break;
    case JPOS: emit_label_op(12, "JPOS", "0100"); break;
    case JZER: emit_label_op(12, "JZER", "0101"); break;
    case JUMP: emit_label_op(12, "JUMP", "0110"); break;
    case LOCO: emit_label_op(12, "LOCO", "0111"); break;
    case LODL:   emit_int_op(12, "LODL", "1000"); break;
    case STOL:   emit_int_op(12, "STOL", "1001"); break;
    case ADDL:   emit_int_op(12, "ADDL", "1010"); break;
    case SUBL:   emit_int_op(12, "SUBL", "1011"); break;
    case JNEG: emit_label_op(12, "JNEG", "1100"); break;
    case JNZE: emit_label_op(12, "JNZE", "1101"); break;
    case CALL: emit_label_op(12, "CALL", "1110"); break;
    case PSHI: emit_fixed_op( 0, "PSHI", "1111000"); break;
    case POPI: emit_fixed_op( 0, "POPI", "1111001"); break;
    case PUSH: emit_fixed_op( 0, "PUSH", "1111010"); break;
    case  POP: emit_fixed_op( 0,  "POP", "1111011"); break;
    case RETN: emit_fixed_op( 0, "RETN", "1111100"); break;
    case SWAP: emit_fixed_op( 0, "SWAP", "1111101"); break;
    case INSP:   emit_int_op( 8, "INSP", "11111100"); break;
    case DESP:   emit_int_op( 8, "DESP", "11111110"); break;
    case HALT: emit_fixed_op( 0, "HALT", "11111111"); break;

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
}

int main(int argc, char *argv[]) {
  int object_file = 0;
  if (argc > 1 && (strncmp(argv[1], "-o", 2) == 0)) {
    object_file = 1;
  }

  char *passone = "/tmp/masm.passone";
  p1 = fopen(passone, "w+");
  if ( p1 == NULL ) {
    fprintf(stderr, "cannot open passone file, %s\n", passone);
    exit(1);
  }
  unlink(passone);
  generate_first_pass();
  
  if (object_file) {
    print_first_pass();
    append_table();
  } else {
    generate_code();
    dump_table();
  }
 
  return 0;
}

#endif

void print_first_pass() {
  char inbuf[81];

  rewind(p1);
  while (fgets(inbuf, 80, p1) != NULL) {
    printf("   %s", inbuf);
  }
}

#define MAX_SYM_LEN 26
typedef struct symtab_entry {
  char name[MAX_SYM_LEN];
  int addr;
  struct symtab_entry *next;
} symtab_entry_t;

symtab_entry_t *symtab = NULL;

void dump_table(void) {
  FILE *fd;
  fflush(stdout);
  fd = popen("sort", "w");
  if ( fd == NULL ) {
    fprintf(stderr, "cannot open sort\n");
    exit(1);
  }
  for (symtab_entry_t *list = symtab; list != NULL; list = list->next) {
    fprintf(fd,"# %-25s %4d\n",list->name, list->addr);
  }
  pclose(fd);
}

void append_table(void) {
  printf("  %d %s\n", 4096, "x");
  for (symtab_entry_t *list = symtab; list != NULL; list = list->next) {
    printf("    %-25s %4d\n",list->name, list->addr);
  }
}

int get_sym_val(char *symbol) {
  for (symtab_entry_t *list = symtab; list != NULL; list = list->next) {
    if (strncmp(list->name, symbol, MAX_SYM_LEN) == 0) {
      return(list->addr);
    }
  }
  return (-1);
}

void generate_code() {
  char instruction[18] = {'0'};
  int  pc = 0, sym_val = 0, old_pc = 0, diff = 0;
  char symbol[26] = {'0'};

  rewind(p1);

  while (fscanf(p1,"%d %s", &pc, instruction) != EOF) {
    if ((diff = pc - old_pc ) > 1) {
      for (int j=1; j<diff; j++) {
        printf("1111111111111111\n");
      }
    }
    old_pc = pc;

    if (instruction[0] == 'U') {
      fscanf(p1, "%s", symbol);
      if ((sym_val = get_sym_val(symbol)) == -1) {
        fprintf(stderr, "no symbol in symbol table: %s\n", symbol);
        exit(27);
      }

      str_n(12, sym_val);
      for (int i=0; i<12; i++) {
        instruction[i+5] = cstr_16[i];
      }
      
      printf("%s\n", &instruction[1]);
    } else {
      printf("%s\n", instruction);
    }
  }
  fclose(p1);
}

void update_sym_table(char *symbol) {
  for (symtab_entry_t *list = symtab; list; list = list->next) {
    if ((strncmp(list->name, symbol, MAX_SYM_LEN)) == 0) {
      list->addr = pc;
      return;
    }
  }
  fprintf(stderr, "error from symbol table on line %d\n", pc);
  exit(27);
}

void search_sym_table(char *symbol) {
  for (symtab_entry_t *list = symtab; list; list = list->next) {
    if (strncmp(list->name, symbol, MAX_SYM_LEN) == 0) {      
      return;
    }
  }
  symtab_entry_t *element = malloc(sizeof(symtab_entry_t));
  if ( element == NULL ) {
    fprintf(stderr, "cannot allocate symtab entry\n");
    exit(1);
  }
  strncpy(element->name, symbol, MAX_SYM_LEN);
  element->next = symtab;
  symtab = element;
}
