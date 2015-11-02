#include <stdio.h>
#include <strings.h>
#include "globals.h"

#define  MemoryChipSize 1024
#define  MOD %

typedef  int ChipRange ;
typedef  DataBusType Memory_Chip[MemoryChipSize] ;

extern int Set_blocking_io();
extern int Set_nonblocking_io();

extern int  polled_io;
extern char input_char;

Memory_Chip MemoryChip0 ;
Memory_Chip MemoryChip1 ;
Memory_Chip MemoryChip2 ;
Memory_Chip MemoryChip3 ;

int        MemorySlot ;
ChipRange  Offset ; 

char       btoc();

ActivateMemoryChip0 (Offset, Data, ReadBit, WriteBit)
ChipRange   Offset ;
DataBusType Data ;
Bit         ReadBit ;
Bit         WriteBit ;   

{
int I ;				/* loop counter index */

          if (ReadBit == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 Data[I] = MemoryChip0[Offset][I] ;
          else if (WriteBit == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 MemoryChip0[Offset][I] = Data[I] ;

}			/* END ActivateMemoryChip0 */

ActivateMemoryChip1 (Offset, Data, ReadBit, WriteBit)

ChipRange   Offset ;
DataBusType Data ;
Bit         ReadBit ;
Bit         WriteBit ;   

{
int I ;				/* loop counter index */

          if (ReadBit  == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 Data[I] = MemoryChip1[Offset][I] ;
          else if (WriteBit == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 MemoryChip1[Offset][I] = Data[I] ;

}			/* END ActivateMemoryChip1 */

ActivateMemoryChip2 (Offset, Data, ReadBit, WriteBit)

ChipRange   Offset ;
DataBusType Data ;
Bit         ReadBit ;
Bit         WriteBit ;   

{
int I ;				/* loop counter index */

          if (ReadBit  == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 Data[I] = MemoryChip2[Offset][I] ;
          else if (WriteBit == One) 
	      for (I = 0 ; I < DataWordSize-1 ; I++) 
		 MemoryChip2[Offset][I] = Data[I] ;

}			/* END ActivateMemoryChip2 */


ActivateMemoryChip3 (Offset, Data, ReadBit, WriteBit)

ChipRange   Offset ;
DataBusType Data ;
Bit         ReadBit ;
Bit         WriteBit ;   

{
  static int receiver_buffer_read = 0;
  int I ;				/* loop counter index */

  if (ReadBit == One) { 
    switch(Offset){
    default:	for (I = 0 ; I < DataWordSize-1 ; I++)
        Data[I] = MemoryChip3[Offset][I] ;
      break;
      
      // Reciever buffer
    case 1020:
      if ( FourthSubcycle() ) {
        receiver_buffer_read++;
      }
      if ( receiver_buffer_read == 2 && MemoryChip3[1021][14] == '1'
         && MemoryChip3[1021][12] == '1') {
        True_ascii_to_mem_ascii(&MemoryChip3[1020][0], &input_char);
        MemoryChip3[1021][15] = '1';
        MemoryChip3[1021][14] = '0';
        polled_io = 2;
        receiver_buffer_read = 0;
        //printf("%d.%d read receiver buffer read (%d) (%c) poll(%d)\n", Cycle(), Subcycle(), input_char, input_char, polled_io);
      }
      for (I = 0 ; I < DataWordSize-1 ; I++)
        Data[I] = MemoryChip3[Offset][I] ;
      break;

    }

    
  } else if (WriteBit == One) {

    switch(Offset){
    default:    for (I = 0 ; I < DataWordSize-1 ; I++)
        MemoryChip3[Offset][I] = Data[I] ;
      break;

      // Reciever buffer
    case 1020: break;

      // Receiver status
    case 1021:
      if (FourthSubcycle()) {
        if (Data[12] == '1') {
          for (I = 0 ; I < DataWordSize-1 ; I++)
            MemoryChip3[Offset][I] = '0';
          polled_io = polled_io ? 2 : 1;
          //printf("%d.%d write receiver status ON, poll(%d)\n", Cycle(), Subcycle(), polled_io);
          MemoryChip3[Offset][14] = '0';
          MemoryChip3[Offset][15] = '1';
          MemoryChip3[Offset][12] = '1';
        } else {
          for (I = 0 ; I < DataWordSize-1 ; I++)
            MemoryChip3[Offset][I] = '0';
          polled_io = 0;
          //printf("%d.%d write receiver status OFF, poll(%d)\n", Cycle(), Subcycle(), polled_io);
        }
      }
      break;

      // Transmit Buffer
    case 1022:
      if (FirstSubcycle() &&  MemoryChip3[1023][12]  =='1'
          &&  MemoryChip3[1023][14]  =='1') {
        for (I = 0 ; I < DataWordSize-1 ; I++)
          MemoryChip3[Offset][I] = Data[I] ;
        fprintf(stdout, "%c", btoc(Data));
        fflush(stdout);
        MemoryChip3[1023][14] = '1';
        MemoryChip3[1023][15] = '0';
      }
      break;

      // Transmit status
    case 1023:
      if (Data[12] == '1') {
        for (I = 0 ; I < DataWordSize-1 ; I++)
          MemoryChip3[Offset][I] = '0';
        MemoryChip3[Offset][14] = '1';
        MemoryChip3[Offset][15] = '0';
        MemoryChip3[Offset][12] = '1';
      } else {
        for (I = 0 ; I < DataWordSize-1 ; I++)
          MemoryChip3[Offset][I] = '0';
      }
      break;
      
    }
  }
}			/* END ActivateMemoryChip3 */


ComputeOffset (Address)
AddressBusType Address ;

{

int  m    ;			/* loop counter index            */
int  temp ;           		/* temp decimal for calculations */
int  Offset ;			/* decimal equivalent of binary  */

   Offset = 0 ;

   for (m = 2 ; m < (AddrWordSize - 1) ; m++)  /*   10 bit offset  */
     { 
       if (Address[m] == '1')
	    temp = 1 ;
       else temp = 0 ;
 
       Offset = (2 * Offset) + temp ;
     }

    return (Offset) ;

}			/* END ComputeOffset */

ComputeChipSelect (Address0, Address1)
Bit Address0, Address1 ;

{

    if ((Address0 == Zero) && (Address1 == Zero)) 
	 return (0) ;
    else if ((Address0 == Zero) && (Address1 == One))
         return (1) ;
    else if ((Address0 == One) && (Address1 == Zero))
         return (2) ;
    else return (3) ;

}			/* END ComputeChipSelect */

void ActivateMemory (Address, Data, ReadBit, WriteBit)
AddressBusType Address ;
DataBusType    Data ;
Bit            ReadBit ;
Bit            WriteBit ;

{
int        ChipNumber ;

    Offset = ComputeOffset (Address) ;

    ChipNumber = ComputeChipSelect (Address[0], Address[1]) ;

    switch (ChipNumber)
    {
       case 0 : ActivateMemoryChip0 (Offset, Data, ReadBit, WriteBit) ;
		  break ;
       case 1 : ActivateMemoryChip1 (Offset, Data, ReadBit, WriteBit) ;
		  break ;
       case 2 : ActivateMemoryChip2 (Offset, Data, ReadBit, WriteBit) ;
		  break ;
       case 3 : ActivateMemoryChip3 (Offset, Data, ReadBit, WriteBit) ;
		  break ;
    }

}			/* END ActivateMemory */

void DumpMemory (From, To)
int From, To ;

{

   int MemorySlot ;
   int Offset ;
   int ChipNumber ;

    for (MemorySlot = From ; MemorySlot <= To ; MemorySlot++) 
    {
        ChipNumber = MemorySlot / MemoryChipSize ;
        Offset     = MemorySlot MOD MemoryChipSize ;

        if (ChipNumber == 0) 
	    printf ("%s", MemoryChip0[Offset]) ;
        if (ChipNumber == 1) 
	    printf ("%s", MemoryChip1[Offset]) ;
        if (ChipNumber == 2) 
	    printf ("%s", MemoryChip2[Offset]) ;
        if (ChipNumber == 3) 
	    printf ("%s", MemoryChip3[Offset]) ;
       
	printf ("\n") ;
    }

}			/* END DumpMemory */

void WriteChip(const char *src) {
  int        ChipNumber ;
  char *dest;

  ChipNumber = MemorySlot / MemoryChipSize ;
  Offset     = MemorySlot MOD MemoryChipSize ;
       if (ChipNumber == 0) dest = MemoryChip0[Offset];
  else if (ChipNumber == 1) dest = MemoryChip1[Offset];
  else if (ChipNumber == 2) dest = MemoryChip2[Offset];
  else if (ChipNumber == 3) dest = MemoryChip3[Offset];

  strcpy(dest, src);
}

void InitializeMemory (program_file)
char *program_file;
{
  FILE *inputfile, *fopen () ;	/* pointer to input file  library   */
  /* function to OPEN file for input  */
  char src[1024];
   
  for (MemorySlot = 0 ; MemorySlot <= 4095 ; MemorySlot++) {
    WriteChip("1111111111111111");
  }
    
  strcpy (MemoryChip3[1020], "0000000000000000");
  strcpy (MemoryChip3[1021], "0000000000000000");
  strcpy (MemoryChip3[1022], "0000000000000000");
  strcpy (MemoryChip3[1023], "0000000000000000");

  /* open program file for loading                */

  if((inputfile = fopen (program_file, "r")) == NULL){
    if((inputfile = fopen ("inner.dat", "r")) == NULL){
      fprintf(stderr,"Can't open Program File, aborting \n");
      exit(2);
    }
  }

  for (MemorySlot = 0 ; MemorySlot < (MemoryChipSize * 4); MemorySlot++) {
  init_start: if((fscanf(inputfile, "%s", src)) == EOF) break;
    if(src[0] == '#') {
      fscanf(inputfile, "%s", src);
      fscanf(inputfile, "%s", src);
      goto init_start;
    }

    WriteChip(src);
  }

  fprintf(stderr,"Read in %d machine instructions\n", MemorySlot);
  fclose (inputfile) ;    
}			/* END InitializeMemory */

void    MemoryRead(mem_loc, mem_val)
        int  mem_loc;
        char *mem_val;
{
       switch(mem_loc / 1024){
            case 0: strncpy(mem_val, MemoryChip0[mem_loc], 16);
                    mem_val[17] = '\0';
                    break;
            case 1: strncpy(mem_val, MemoryChip1[mem_loc - 1024], 16);
                    mem_val[17] = '\0';
                    break;
            case 2: strncpy(mem_val, MemoryChip2[mem_loc - 2048], 16);
                    mem_val[17] = '\0';
                    break;
            case 3: strncpy(mem_val, MemoryChip3[mem_loc - 3072], 16);
                    mem_val[17] = '\0';
                    break;
      }

}
