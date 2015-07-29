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

char input_characters[100][80];
char *inbuf;

int  input_x=0, input_y=0, input_buf=0;

int  original_stdin_channel_flags;
int  nonblock_stdin_channel_flags;

/**** call sequence:   mic1 promfile_name programfile_name pc sp    ****/

main (argc, argv)
int  argc;
char *argv[];
{

DataBusType    Data ;
AddressBusType Address ;
Bit ReadBit ;
Bit WriteBit ;
int ClockCycle, mem_location, ml, i, j, m, col, mem_offset;
char query[80];
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


     for(i=0; i<100; i++){
	for(j=0; j<80; j++)input_characters[i][j] = '\0';
     }

     BurnInProm (promfile);
     InitializeMemory (programfile) ;
     InitializePCandStackPointer (pc, sp) ;
     strcpy (Address, "000000000000") ;
     strcpy (Data,    "0000000000000000") ;
     ReadBit  = Zero ;
     WriteBit = Zero ;

tag: for ( ; ; ) 
     { 
	if(polled_io){
		Set_nonblocking_io();
		if((inbuf = fgets(&input_characters[input_buf][0], 99, stdin))){
		  input_characters[input_buf][col=strlen(inbuf)] = '\n';
		  input_characters[input_buf][col+1] = '\0';
		  input_buf = (input_buf + 1) % 100;
		  polled_io = 0;
		  MemoryChip3[1021][14] = '1';
		  MemoryChip3[1021][15] = '0';
		  Set_blocking_io();
		}
	}

        GeneratePulse () ;

        ActivateCpu (Address, Data, &ReadBit, &WriteBit) ;  

        ActivateMemory (Address, Data, ReadBit, WriteBit) ;  

	if ((ReadBit == One) && (WriteBit == One)){
	   sleep(1);
	   Set_blocking_io();
	   tcflush(0, TCIFLUSH);
	   break ;
	}
     }

    DumpRegisters () ; 
    ClockCycle = Cycle () ;
    printf ("\nTotal cycles   : %d\n\n", ClockCycle);
    tcflush(0, TCIFLUSH);  /* dump anything in the input queue */
/***********
    printf("If you would like to examine memory enter  y  if not enter  n: ");
    gets(query);
***********/
    if(1 || query[0] == 'y' || query[0] == 'Y'){
      while(1){
	   printf("Type decimal address to view memory,  q  to quit or  c  to continue: ");
	   fgets(query, 79, stdin);
	   if(query[0] == 'c'){
/*
	      mem_location = (btoi(ProgramCounter) + 1);
	      for(i=15; i>=0; i--){
                if(mem_location >=  power2[i]){
                      ProgramCounter[15-i] = '1';
                      mem_location -= power2[i];
                }else{
                      ProgramCounter[15-i] = '0';
                }
              }
	      ProgramCounter[16] = '\0';
*/
	      printf("\nThe new PC is  : %s\n\n", ProgramCounter);
	      MicroPc = 0 ;
	      Quartz.Subcycle = 0;
              goto tag;
	   }
	   if(query[0] == 'q' || query[0] == 'Q'){
	      printf("MIC-1 emulator finishing, goodbye\n\n");
	      exit(1);
	   }else{
	      ml = mem_location = atoi(query);
	      if(mem_location < 0 || mem_location > 4095){
		printf("BAD LOCATION VALUE, MUST BE BETWEEN 0 and 4095\n");
		continue;
	      }
	      for(i=11; i>=0; i--){
		if(mem_location >=  power2[i]){
		      Address[11-i] = '1';
		      mem_location -= power2[i];
		}else{
		      Address[11-i] = '0';
	        }
	      }
	      Address[12] = '\0';
	      ActivateMemory (Address, mem_loc, '1', '0');

	      mem_loc[16] = '\0';
	      printf("     the location %4d has value %16s , or %5d  or signed %6d\n",
                               ml, mem_loc, btoi(mem_loc), (short)btoi(mem_loc));
	      printf("Type  <Enter>  to continue debugging\nType        q  to quit\nType        f for forward range\nType        b for backward range: ");
		fgets(query, 79, stdin);
           if(query[0] == 'q' | query[0] == 'Q'){
              printf("MIC-1 emulator finishing, goodbye\n\n");
              exit(1);
           }else{
		if (query[0] == 'f' || query[0] == 'F'){
			printf("Type the number of forward locations to dump: ");
			fgets(query, 79, stdin);
			mem_offset = atoi(query); 
			if (ml + mem_offset > 4091){
			  printf("BAD OFFSET VALUE, GOES BEYOND 4091\n");
                	  continue;
			}
			  /********************** continue here *******/
			for(m=0; m<mem_offset; m++){ 
			 mem_location = ml + m + 1;
                         for(i=11; i>=0; i--){
                           if(mem_location >=  power2[i]){
                             Address[11-i] = '1';
                             mem_location -= power2[i];
                           }else{
                             Address[11-i] = '0';
                           }
                         }
                         Address[12] = '\0';
                         ActivateMemory (Address, mem_loc, '1', '0');

                         mem_loc[16] = '\0';
                         printf("     the location %4d has value %16s , or %5d  or signed %6d\n" ,
                          (ml + m + 1), mem_loc, btoi(mem_loc), (short)btoi(mem_loc));
			}
		 }else{	  
                      if (query[0] == 'b' || query[0] == 'B'){
                        printf("Type the number of reverse locations to dump: ");
			fgets(query, 79, stdin);
                        mem_offset = atoi(query);
                        if (ml - mem_offset < 0){
                          printf("BAD OFFSET VALUE, GOES BELOW 0\n");
                          continue;
                        }
                          /********************** continue here *******/
                        for(m=0; m<mem_offset; m++){
                         mem_location = (ml - (m + 1));
                         for(i=11; i>=0; i--){
                           if(mem_location >=  power2[i]){
                             Address[11-i] = '1';
                             mem_location -= power2[i];
                           }else{
                             Address[11-i] = '0';
                           }
                         }
                         Address[12] = '\0';
                         ActivateMemory (Address, mem_loc, '1', '0');

                         mem_loc[16] = '\0';
                         printf("     the location %4d has value %16s , or %5d  or signed %6d\n" ,
                          (ml - (m + 1)), mem_loc, btoi(mem_loc), (short)btoi(mem_loc));
			}
		       }else continue;
		   }
		}
		continue;
/**************************************************************************/
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
