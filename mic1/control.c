#include <stdio.h>
#include <strings.h>
#include "globals.h"

extern  FirstSubcycle () ;
extern  SecondSubcycle () ;
extern  ThirdSubcycle () ; 
extern  FourthSubcycle () ;

#define MicroWordSize 33
#define MaxSize       256
#define Blank         ' ' 
#define MicroPcSize   9

typedef Bit FourBits[5] ;
typedef Bit EightBits[9] ;
typedef Bit MicroWord[MicroWordSize] ;


MicroWord MicroMemory[MaxSize] ;
int       MicroPc = 0 ;
MicroWord MIR ;
int       RowCounter = 0;

void BurnInProm (prom_file)
char *prom_file;
{

int  Row ;
int  Col ;
char code[MicroWordSize] ;	/* width of control store code      */
FILE *inputfile, *fopen () ;	/* pointer to input file library    */
				/* function to OPEN file for input  */

   for (Row = 0 ; Row < MaxSize ; Row++)
      for (Col = 0 ; Col < MicroWordSize-1 ; Col++)
         MicroMemory[Row][Col] = Zero ;

   /* read code from file line by line         */
   /* and assign the code to the control store */

   Row = 0 ;

   /* open promfile for loading                */

   if((inputfile = fopen (prom_file, "r")) == NULL){
      if((inputfile = fopen ("promfile.dat", "r")) == NULL){
         fprintf(stderr,"Can't open Promfile, aborting \n");
         exit(2);
      }
   }

   /* read in code LINE BY LINE and scan it */

   while (fscanf (inputfile, "%s", code) != EOF) 
      {
         for (Col = 0 ; Col < MicroWordSize-1 ; Col++) 
            MicroMemory[Row][Col] = code[Col] ;
	 Row++ ;
         RowCounter++;
      }

    fprintf(stderr,"\nRead in %d micro instructions\n", RowCounter);
    fclose (inputfile)  ;
	
}			/* END BurnInProm */


OutputProm () 
{

    int Row ;
    int Col ;

        for (Row = 0 ; Row < RowCounter ; Row++) 
	   {
              for (Col = 0 ; Col < MicroWordSize-1 ; Col++) 
                  printf ("%c", MicroMemory[Row][Col]) ;
	      printf ("\n") ;
           }
}			/* END OutputProm */


BusRegister (RField)
FourBits RField ;
{
   int Sum ;

   Sum = 0 ;
   if (RField[3] == One) 
       Sum = Sum + 1 ;
   if (RField[2] == One)
       Sum = Sum + 2 ;
   if (RField[1] == One)
       Sum = Sum + 4 ;
   if (RField[0] == One)
       Sum = Sum + 8 ;

   return (Sum) ;
    
}		/* END BusRegister */

DecodeRegField (RField, Field)
FourBits    RField ;
DataBusType Field ;

{
int Temp ;

    Temp = BusRegister (RField);

    switch (Temp)
    {
         case 0   :  strcpy (Field, "1000000000000000") ;
		     break ;
         case 1   :  strcpy (Field, "0100000000000000") ;
		     break ;
         case 2   :  strcpy (Field, "0010000000000000") ;
		     break ;
         case 3   :  strcpy (Field, "0001000000000000") ;
		     break ;
         case 4   :  strcpy (Field, "0000100000000000") ;
		     break ;
         case 5   :  strcpy (Field, "0000010000000000") ;
		     break ;
         case 6   :  strcpy (Field, "0000001000000000") ;
		     break ;
         case 7   :  strcpy (Field, "0000000100000000") ;
		     break ;
         case 8   :  strcpy (Field, "0000000010000000") ;
		     break ;
         case 9   :  strcpy (Field, "0000000001000000") ;
		     break ;
         case 10  :  strcpy (Field, "0000000000100000") ;
		     break ;
         case 11  :  strcpy (Field, "0000000000010000") ;
		     break ;
         case 12  :  strcpy (Field, "0000000000001000") ;
		     break ;
         case 13  :  strcpy (Field, "0000000000000100") ;
		     break ;
         case 14  :  strcpy (Field, "0000000000000010") ;
		     break ;
         case 15  :  strcpy (Field, "0000000000000001") ;      
		     break ;
    }		     /* end case structure */

}			/* END DecodeRegField */

DecodeAField (ABits) 
DataBusType  ABits ;
{
FourBits AField ;

        AField[0]  =  MIR[20];
        AField[1]  =  MIR[21];
        AField[2]  =  MIR[22];
        AField[3]  =  MIR[23];
        DecodeRegField (AField, ABits) ;

}			/* END DecodeAField */

DecodeBField (BBits)
DataBusType BBits ;
{
FourBits BField ;

        BField[0]  =  MIR[16];
        BField[1]  =  MIR[17];
        BField[2]  =  MIR[18];
        BField[3]  =  MIR[19];
        DecodeRegField (BField, BBits) ;

}			/* END DecodeBField */

DecodeCField (CBits)
DataBusType CBits ;
{
FourBits CField ;

        if (MIR[11] == One)
        {
            CField[0]  =  MIR[12];
            CField[1]  =  MIR[13];
            CField[2]  =  MIR[14];
            CField[3]  =  MIR[15];
            DecodeRegField (CField, CBits) ;
        }
        else strcpy (CBits, "0000000000000000") ;

}			/* END DecodeCField */

LoadMirFromControlStore ()
    
{ 
int I ;

	for (I = 0 ; I < MicroWordSize-1 ; I++)
	   MIR[I] = MicroMemory[MicroPc][I] ;

}			/* END LoadMirFromControlStore */

DetermineMmux (NBit, ZBit, Cond, Mmux) 
Bit     NBit, ZBit ;
TwoBits Cond ;
Bit     *Mmux ;

{
Bit CondBit0 ;
Bit CondBit1 ;

        CondBit0 = Cond[0] ;    
	CondBit1 = Cond[1] ;

        if ((CondBit0 == Zero) && (CondBit1 == Zero))
	    *Mmux = Zero ;
        if ((CondBit0 == One) && (CondBit1 == One))
	    *Mmux = One  ;
        if ((CondBit0 == One) && (CondBit1 == Zero)) 
            if (ZBit == One) 
	         *Mmux = One ;
   	    else *Mmux = Zero ;

        if ((CondBit0 == Zero) && (CondBit1 == One))
           if (NBit == One) 
	        *Mmux = One ;
	   else *Mmux = Zero ;

}			/* END DetermineMmux */

ConvertToCardinal (Addr)
EightBits Addr ;

{
   int  I ;		/* 0..7 */
   int  temp ;          /* temp decimal for calculations */
   int  Sum ;		/* decimal equivalent of binary  */

   Sum = 0 ;

   for (I = 0 ; I <= 7 ; I++) 
     { 
       if (Addr[I] == One)
	    temp = 1 ;
       else temp = 0 ;
 
       Sum = (2 * Sum) + temp ;
     }
   return (Sum) ;

}			/* END ConvertToCardinal */

LoadMicroProgramCounter (NBit, ZBit, Cond, Addr) 
Bit       NBit, ZBit ;
TwoBits   Cond ;
EightBits Addr ;

{
    Bit Mmux ;


   DetermineMmux (NBit, ZBit, Cond, &Mmux) ;

   if (Mmux == Zero) 
         MicroPc = MicroPc + 1 ;
   else
	 MicroPc = ConvertToCardinal (Addr) ;

}			/* END LoadMicroProgramCounter */

ActivateControlStore (NBit, ZBit, ABits, BBits, CBits,
                      AmuxBit, AluBits, ShiftBits, 
                      MbrBit, MarBit, ReadBit, WriteBit)

Bit         NBit, ZBit ;
DataBusType ABits, BBits, CBits ;
Bit         *AmuxBit ;
TwoBits     AluBits, ShiftBits ;
Bit         *MbrBit ;
Bit         *MarBit ;
Bit         *ReadBit ;
Bit         *WriteBit ;

{
TwoBits   Cond ;
EightBits Addr ;
/*FourBits  AField ;
FourBits  BField ;
FourBits  CField ;*/

        if (FirstSubcycle ()) 
	   {
	       LoadMirFromControlStore () ; 

               *AmuxBit     =  MIR[0] ;
               AluBits[0]   =  MIR[3] ;
               AluBits[1]   =  MIR[4] ;
               ShiftBits[0] =  MIR[5] ;
               ShiftBits[1] =  MIR[6] ;
               *MbrBit      =  MIR[7] ;
               *MarBit      =  MIR[8] ;
               *ReadBit     =  MIR[9] ;
               *WriteBit    =  MIR[10] ;
	   }

        if (SecondSubcycle())
           {
               DecodeAField (ABits) ;
               DecodeBField (BBits) ;
               *AmuxBit     =  MIR[0] ;
               AluBits[0]   =  MIR[3] ;
               AluBits[1]   =  MIR[4] ;
               ShiftBits[0] =  MIR[5] ;
               ShiftBits[1] =  MIR[6] ;
               *MbrBit      =  MIR[7] ;
               *MarBit      =  MIR[8] ;
               *ReadBit     =  MIR[9] ;
               *WriteBit    =  MIR[10] ;
	   }

        if (ThirdSubcycle ())
	   {
               *AmuxBit     =  MIR[0] ;
               AluBits[0]   =  MIR[3] ;
               AluBits[1]   =  MIR[4] ;
               ShiftBits[0] =  MIR[5] ;
               ShiftBits[1] =  MIR[6] ;
               *MbrBit      =  MIR[7] ;
               *MarBit      =  MIR[8] ;
               *ReadBit     =  MIR[9] ;
               *WriteBit    =  MIR[10] ;
	   }

        if (FourthSubcycle()) 
	   {
               DecodeCField (CBits) ; 
               Cond[0] = MIR[1] ; Cond[1] = MIR[2] ;
               Addr[0] = MIR[24] ; Addr[1] = MIR[25] ;
               Addr[2] = MIR[26] ; Addr[3] = MIR[27] ;
               Addr[4] = MIR[28] ; Addr[5] = MIR[29] ;
               Addr[6] = MIR[30] ; Addr[7] = MIR[31] ;

               LoadMicroProgramCounter (NBit, ZBit, Cond, Addr) ;

               *AmuxBit     =  MIR[0] ;
               AluBits[0]   =  MIR[3] ;
               AluBits[1]   =  MIR[4] ;
               ShiftBits[0] =  MIR[5] ;
               ShiftBits[1] =  MIR[6] ;
               *MbrBit      =  MIR[7] ;
               *MarBit      =  MIR[8] ;
               *ReadBit     =  MIR[9] ;
               *WriteBit    =  MIR[10] ;
	   }

}			/* END ActivateControlStore */
