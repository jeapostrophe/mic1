#include <stdio.h>
#include "globals.h" 
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include <termios.h>

#define  MemoryChipSize 1024
typedef  DataBusType Memory_Chip[MemoryChipSize] ;


void Set_blocking_io();
void Set_nonblocking_io();
extern void BurnInProm () ;
extern void InitializeMemory () ;
extern void InitializePCandStackPointer () ;
extern void ActivateCpu () ;  
extern void ActivateMemory () ;
extern void DumpMemory () ;
extern void GeneratePulse () ;
extern int Cycle () ;

void InitializeSymbolTable(char *);
void ShowSymbolTable();
const char *LookupSymbol(char *);

struct Clock
      {
         int Cycle ;
         int Subcycle ;         /* 0..4 */
      } ;


extern struct Clock Quartz;
extern DataBusType    ProgramCounter ;
extern int            MicroPc;

extern Memory_Chip MemoryChip3 ;

int btoi();

int  power2[16] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768};

int  polled_io = 0;   /* flag for non-blocking input  */
char input_char = 0;

int  original_stdin_channel_flags;
int  nonblock_stdin_channel_flags;

/**** call sequence:   mic1 promfile_name programfile_name pc sp    ****/

#define QUERY_LEN 80
char query[QUERY_LEN];
void debugger_read() {
  memset(query, 0, QUERY_LEN);
  fgets(query, QUERY_LEN, stdin);
}

int ensure_valid_addr(int mem_location) {
  if (mem_location < 0 || mem_location > 4095) {
    printf("BAD LOCATION VALUE, MUST BE BETWEEN 0 and 4095\n");
    return 1;
  }
  return 0;
}

void debugger_read_mem_loc(int ml) {
  AddressBusType Address;
  char mem_loc[17];

  int mem_location = ml;
  for (int i=11; i>=0; i--) {
    if (mem_location >= power2[i]) {
      Address[11-i] = '1';
      mem_location -= power2[i];
    } else {
      Address[11-i] = '0';
    }
  }
  Address[12] = '\0';
  ActivateMemory (Address, mem_loc, '1', '0');

  mem_loc[16] = '\0';
  printf("     the location %4d has value %16s , or %5d  or signed %6d\n", ml, mem_loc, btoi(mem_loc), (short)btoi(mem_loc));
}

void debugger_show_locations(int ml, int low_mult, int hi_mult, const char *label) {
  printf("Type the number of %s locations to dump: ", label);
  debugger_read();
  
  int mem_span_magnitude = atoi(query);

  int lo_addr = ml - low_mult*mem_span_magnitude;
  int hi_addr = ml + hi_mult*mem_span_magnitude;
  if (ensure_valid_addr(lo_addr)) return;
  if (ensure_valid_addr(hi_addr)) return;

  for (int m = lo_addr; m <= hi_addr; m++) { 
    debugger_read_mem_loc(m);
  }
}

main (argc, argv)
int  argc;
char *argv[];
{
  DataBusType    Data ;
  AddressBusType Address;
  Bit ReadBit ;
  Bit WriteBit ;
  int ClockCycle, mem_location, ml, i, j, m, col, mem_offset;
  char query_val[QUERY_LEN];
  char mem_loc[17];

  char   promfile[80];
  char   programfile[80];
  int    pc, sp;

  if((original_stdin_channel_flags = fcntl(0, F_GETFL, 0)) == -1){
	perror("fnctl failed: ");
	exit(1);
  }

  nonblock_stdin_channel_flags = original_stdin_channel_flags | O_NONBLOCK;

  switch(argc){
  case 1:   promfile[0] = '\0';
    programfile[0] =  '\0';
    pc = -1;
    sp = -1;
    break;
  case 2:   strcpy(promfile, argv[1]);
    programfile[0] =  '\0';
    pc = -1;
    sp = -1;
    break;
  case 3:   strcpy(promfile, argv[1]);
    strcpy(programfile, argv[2]);
    pc = -1;
    sp = -1;
    break;
  case 4:   strcpy(promfile, argv[1]);
    strcpy(programfile, argv[2]);
    pc = atoi(argv[3]);
    sp = -1;
    break;
  case 5:   strcpy(promfile, argv[1]);
    strcpy(programfile, argv[2]);
    pc = atoi(argv[3]);
    sp = atoi(argv[4]);
    break;
  default:   fprintf(stderr,"Too many command line arguments, aborting\n");
    exit(2);
  }

  BurnInProm (promfile);
  InitializeMemory (programfile) ;
  InitializeSymbolTable (programfile) ;
  InitializePCandStackPointer (pc, sp) ;
  strcpy (Address, "000000000000") ;
  strcpy (Data,    "0000000000000000") ;
  ReadBit  = Zero ;
  WriteBit = Zero ;

 tag: for ( ; ; ) {
    GeneratePulse () ;

    if (polled_io == 2 && FirstSubcycle() ) {
      Set_nonblocking_io();
      input_char = fgetc(stdin);
      if (input_char != EOF) {
        polled_io = 0;
        //printf("%d.%d read (%d) poll(%d)\n", Cycle(), Subcycle(), input_char, polled_io);
        MemoryChip3[1021][14] = '1';
        MemoryChip3[1021][15] = '0';
        Set_blocking_io();
      }
    } else {
      //printf("%d.%d no read\n", Cycle(), Subcycle());
    }
       
    ActivateCpu (Address, Data, &ReadBit, &WriteBit) ;  
    ActivateMemory (Address, Data, ReadBit, WriteBit) ;  

    // Escape to the debugger
    if ((ReadBit == One) && (WriteBit == One) && ClockCycle != Cycle() ) {
      Set_blocking_io();
      break ;
    }
  }
     
  DumpRegisters () ; 
  ClockCycle = Cycle () ;
  printf ("\nMicroPC        : %d\n", MicroPc);
  printf ("Total cycles   : %d\n\n", ClockCycle);

  while (1) {
    printf("Type address to view memory, [q]uit, [c]ontinue, <Enter> for symbol table:\n");
    debugger_read();
    if (query[0] == 'c') {
      goto tag;
    } else if (query[0] == '\n') {
      ShowSymbolTable();
    } else if (query[0] == 0 || query[0] == 'q' || query[0] == 'Q') {
      break;
    } else {
      sscanf(query, "%s", query_val);
      ml = atoi(LookupSymbol(query_val));
      if (ensure_valid_addr(ml)) continue;
      
      debugger_read_mem_loc(ml);
      
      printf("Type  <Enter>  to continue debugging\nType        q  to quit\nType        f for forward range\nType        b for backward range:\n");
      debugger_read();
      if (query[0] == 0 || query[0] == 'q' || query[0] == 'Q') {
        break;
      } else if (query[0] == 'f' || query[0] == 'F') {
        debugger_show_locations(ml, 0, 1, "forward");
      } else if (query[0] == 'b' || query[0] == 'B') {
        debugger_show_locations(ml, 1, 0, "reverse");
      }
    }
  }
  
  printf("MIC-1 emulator finishing, goodbye\n\n");
  exit(1);
}			/* END Driver */

/* passed an array of bytes of 16 ascii 1s and 0s */
/* and a final 17th NULL byte, and converts the   */
/* information into a true binary integer         */

int btoi(mem_loc)
    char *mem_loc;
{
int i, result;

	result = 0;

	for(i=0; i<16; i++){
	   if(mem_loc[i] == '1')
		result += power2[15-i];
	}
	return result;
}


/* passed an array of bytes of 16 ascii 1s and 0s */
/* and a final 17th NULL byte, and converts the   */
/* information into a true char (8 bit ascii)     */
/* by skipping the first 8 bytes and converting   */
/* the last 8                                     */

char btoc(mem_loc)
    char *mem_loc;
{
int i, result;

        result = 0;

        for(i=8; i<16; i++){
           if(mem_loc[i] == '1')
                result += power2[15-i];
        }
        return ((char)result);
}

/* passed a memory location and a true binary char */
/* will fill low order 8 bit memory location with  */
/* 1s and 0s corresponding to the ascii character  */

int  True_ascii_to_mem_ascii(mem_location, character)
	char  *mem_location;
	char  *character;
{
int  i;

	for(i=0; i<16; i++){
		mem_location[i] = '0';
	}
	mem_location[16] = '\0';
	for(i=8; i<16; i++){
		if(power2[15-i] & (*character)){
		   mem_location[i] = '1';
		}
	}
	return 1;
}

void  Set_blocking_io(){
	if(fcntl(0, F_SETFL, original_stdin_channel_flags) == -1){
	  perror("reset fcntl error: ");
	  exit(2);
	}
}

void  Set_nonblocking_io(){
	if(fcntl(0, F_SETFL, nonblock_stdin_channel_flags) == -1){
	  perror("set blocking fcntl error: ");
	  exit(2);
	}
}

typedef struct SymbolTableEntry {
  char *name;
  char *value;
  struct SymbolTableEntry *next;
} SymbolTableEntry_t;

SymbolTableEntry_t *SymbolTable = NULL;

const char *LookupSymbol(char *name) {
  SymbolTableEntry_t *cur = SymbolTable;
  while (cur != NULL) {
    if (strcmp(name, cur->name) == 0) {
      return cur->value;
    } else {
      cur = cur->next;
    }
  }
  return name;
}

void ShowSymbolTable() {
  SymbolTableEntry_t *cur = SymbolTable;
  while (cur != NULL) {
    printf("\t%s\n", cur->name);
    cur = cur->next;
  }
}

void InitializeSymbolTable(char *program_file) {
  FILE *inputfile;
  if ((inputfile = fopen (program_file, "r")) == NULL) {
      fprintf(stderr,"Can't open Program File, aborting \n");
      exit(2);
  }

  unsigned int count = 0;
  char src[1024];
  char *name;
  char *value;
  SymbolTableEntry_t *new_entry;
  while(1) {
    if ((fscanf(inputfile, "%s", src)) == EOF) break;
    if (src[0] != '#') break;

    fscanf(inputfile, "%s", src);
    name = malloc(sizeof(char)*strlen(src));
    strcpy(name, src);
    
    fscanf(inputfile, "%s", src);
    value = malloc(sizeof(char)*strlen(src));
    strcpy(value, src);

    new_entry = malloc(sizeof(SymbolTableEntry_t));
    new_entry->name = name;
    new_entry->value = value;
    new_entry->next = SymbolTable;    
    SymbolTable = new_entry;
    
    count++;
  }

  fprintf(stderr,"Read in %d symbol table entries\n", count);
  fclose(inputfile);
  return;
}
