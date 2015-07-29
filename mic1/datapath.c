#include <stdio.h>
#include <strings.h>
#include "globals.h"

extern ActivateAlu () ;
extern ActivateShifter () ;
extern FirstSubcycle () ; 
extern SecondSubcycle () ;
extern ThirdSubcycle () ; 
extern FourthSubcycle () ;

DataBusType    ProgramCounter="0000000000000000" ;
DataBusType    Accumulator="0000000000000000" ;
DataBusType    StackPointer="0000000000000000" ;
DataBusType    InstructionReg="0000000000000000" ;
DataBusType    TempInstruction="0000000000000000" ;
DataBusType    ZeroRegister ;
DataBusType    PositiveOne ;
DataBusType    NegativeOne ;
DataBusType    Amask ;
DataBusType    Smask ;
DataBusType    ARegister="0000000000000000" ;
DataBusType    BRegister="0000000000000000" ;
DataBusType    CRegister="0000000000000000" ;
DataBusType    DRegister="0000000000000000" ;
DataBusType    ERegister="0000000000000000" ;
DataBusType    FRegister="0000000000000000" ;
DataBusType    ALatch ;
DataBusType    BLatch ;
AddressBusType MAR ;
DataBusType    MBR ;
DataBusType    AluResult ;
DataBusType    ShifterResult ;

extern int btoi();

DumpRegisters () 

{

     printf ("\nProgramCounter : %s  base 10:  %6d\n", ProgramCounter,
                                              btoi(ProgramCounter)) ;
     printf ("Accumulator    : %s  base 10:  %6d\n", Accumulator,
                                              btoi(Accumulator)) ;
     printf ("InstructionReg : %s  base 10:  %6d\n", InstructionReg,
                                              btoi(InstructionReg)) ;
     printf ("TempInstr      : %s  base 10:  %6d\n", TempInstruction,
                                              btoi(TempInstruction)) ;
     printf ("StackPointer   : %s  base 10:  %6d\n", StackPointer,
                                              btoi(StackPointer)) ;
     printf ("ARegister      : %s  base 10:  %6d\n", ARegister,
                                              btoi(ARegister)) ;
     printf ("BRegister      : %s  base 10:  %6d\n", BRegister,
                                              btoi(BRegister)) ;
     printf ("CRegister      : %s  base 10:  %6d\n", CRegister,
                                              btoi(CRegister)) ;
     printf ("DRegister      : %s  base 10:  %6d\n", DRegister,
                                              btoi(DRegister)) ;
     printf ("ERegister      : %s  base 10:  %6d\n", ERegister,
                                              btoi(ERegister)) ;
     printf ("FRegister      : %s  base 10:  %6d\n", FRegister,
                                              btoi(FRegister)) ;



}			/* END DumpRegisters */

LoadMar (DataLines) 
DataBusType DataLines  ;

{
int I ;

        for (I = 0 ; I <= 11 ; I++)
	   MAR[I] = DataLines[I+4] ;

}			/* END LoadMar */


SelectRegister (Lines)
DataBusType Lines ;

{
       int I ;
       int Register ;

        Register = 16 ;

        for (I = 0 ; I < DataWordSize ; I++) 
            if (Lines[I] == One)
		Register = I ;

        return (Register) ;

} 			/* END SelectRegister */
   
LoadALatch (ABits)
DataBusType ABits ;

{
int  Register ;

        Register = SelectRegister (ABits) ; 
        switch (Register)
	{
               case 0   :  strcpy (ALatch, ProgramCounter) ; 
			   break ;
               case 1   :  strcpy (ALatch, Accumulator) ;   
			   break ;
               case 2   :  strcpy (ALatch, StackPointer) ; 
			   break ;
               case 3   :  strcpy (ALatch, InstructionReg) ;
			   break ;
               case 4   :  strcpy (ALatch, TempInstruction) ;
			   break ;
               case 5   :  strcpy (ALatch, ZeroRegister) ;  
			   break ;
               case 6   :  strcpy (ALatch, PositiveOne) ;  
			   break ;
               case 7   :  strcpy (ALatch, NegativeOne) ; 
			   break ;
               case 8   :  strcpy (ALatch, Amask) ;      
			   break ;
               case 9   :  strcpy (ALatch, Smask) ;     
			   break ;
               case 10  :  strcpy (ALatch, ARegister) ; 
			   break ;
               case 11  :  strcpy (ALatch, BRegister) ;
			   break ;
               case 12  :  strcpy (ALatch, CRegister) ;
			   break ;
               case 13  :  strcpy (ALatch, DRegister) ;
			   break ;
               case 14  :  strcpy (ALatch, ERegister) ;
			   break ;
               case 15  :  strcpy (ALatch, FRegister) ;
			   break ;
	}

}			/* END LoadALatch */

LoadBLatch (BBits) 
DataBusType BBits ;

{

    int Register ;

    Register = SelectRegister (BBits) ;

        switch (Register)
	{
               case 0   :  strcpy (BLatch, ProgramCounter) ; 
			   break ;
               case 1   :  strcpy (BLatch, Accumulator) ;   
			   break ;
               case 2   :  strcpy (BLatch, StackPointer) ; 
			   break ;
               case 3   :  strcpy (BLatch, InstructionReg) ;
			   break ;
               case 4   :  strcpy (BLatch, TempInstruction) ;
			   break ;
               case 5   :  strcpy (BLatch, ZeroRegister) ;  
			   break ;
               case 6   :  strcpy (BLatch, PositiveOne) ;  
			   break ;
               case 7   :  strcpy (BLatch, NegativeOne) ; 
			   break ;
               case 8   :  strcpy (BLatch, Amask) ;      
			   break ;
               case 9   :  strcpy (BLatch, Smask) ;     
			   break ;
               case 10  :  strcpy (BLatch, ARegister) ; 
			   break ;
               case 11  :  strcpy (BLatch, BRegister) ;
			   break ;
               case 12  :  strcpy (BLatch, CRegister) ;
			   break ;
               case 13  :  strcpy (BLatch, DRegister) ;
			   break ;
               case 14  :  strcpy (BLatch, ERegister) ;
			   break ;
               case 15  :  strcpy (BLatch, FRegister) ;
			   break ;
	}

}			/* END LoadBLatch */

LoadRegisterBank (CBits, DataLines)
DataBusType  CBits ; 
DataBusType  DataLines ;

{
int Register ;

        Register = SelectRegister (CBits) ; 

	switch (Register) 
	{
               case 0  :  strcpy (ProgramCounter, DataLines) ; 
			  break ;
               case 1  :  strcpy (Accumulator, DataLines) ;
			  break ;
               case 2  :  strcpy (StackPointer, DataLines) ;
			  break ;
               case 3  :  strcpy (InstructionReg, DataLines) ;
			  break ;
               case 4  :  strcpy (TempInstruction, DataLines) ;
			  break ;
               case 10 :  strcpy (ARegister, DataLines) ;
			  break ;
               case 11 :  strcpy (BRegister, DataLines);
			  break ;
               case 12 :  strcpy (CRegister, DataLines) ;
			  break ;
               case 13 :  strcpy (DRegister, DataLines) ;
			  break ;
               case 14 :  strcpy (ERegister, DataLines) ;
			  break ;
               case 15 :  strcpy (FRegister, DataLines) ;
			  break ;
               case 16 :  /* strcpy (DataLines, DataLines) ; */
			  break ;
        }

}			/* END LoadRegisterBank */

ActivateDataPath (MarRegs, MbrRegs, ABits, BBits, CBits,   
                  AmuxBit, AluBits, ShiftBits, MbrBit, MarBit,
                  NBit, ZBit)    

AddressBusType MarRegs ;
DataBusType    MbrRegs ;
DataBusType    ABits ;
DataBusType    BBits ;
DataBusType    CBits ;
Bit            AmuxBit ;
TwoBits        AluBits ;
TwoBits        ShiftBits ;
Bit            MbrBit ;
Bit            MarBit ;
Bit            *NBit ;
Bit            *ZBit ;

{

    DataBusType LeftOperand ;
    DataBusType RightOperand ;

    strcpy (MAR, MarRegs) ;
    strcpy (MBR, MbrRegs) ;

    if (SecondSubcycle())
       {
          LoadALatch (ABits) ; 
          LoadBLatch (BBits) ;
       }

    if (ThirdSubcycle())
    {
        if (AmuxBit == One) 
	     strcpy (LeftOperand, MBR) ;
        else strcpy (LeftOperand, ALatch) ;

        if (MarBit == One) 
	{
	     LoadMar (BLatch) ;  
	     strcpy (MarRegs, MAR) ;
	}

        strcpy (RightOperand, BLatch) ;  
        ActivateAlu (LeftOperand, RightOperand, AluBits, AluResult, 
                     NBit, ZBit) ;
        ActivateShifter (AluResult, ShiftBits, ShifterResult) ;

    }

    if (FourthSubcycle()) 
    {
       LoadRegisterBank( CBits, ShifterResult) ; 
       if (MbrBit == One) 
       {
	  strcpy (MBR, ShifterResult) ;
          strcpy (MbrRegs, ShifterResult) ;
       }
    }

}			/* END ActivateDataPath */

void InitializePCandStackPointer (pc, sp)
int pc, sp;
{
   char pc_str[17], sp_str[17];
   int i, mask = 0x00008000;

   if(pc < 0 || sp < 0 || pc > 2047 || sp > 4095){
      strcpy (ProgramCounter, "0000000000000000") ;
      strcpy (StackPointer,   "0000111110000000") ;
   }else{
      for(i=0; i<16; i++){
          if(pc & (mask >> i))
             pc_str[i] = '1';
          else
             pc_str[i] = '0';
          if(sp & (mask >> i))
             sp_str[i] = '1';
          else
             sp_str[i] = '0';
      }
      pc_str[16] = '\0';
      sp_str[16] = '\0';
      strcpy (ProgramCounter,  pc_str);
      strcpy (StackPointer,    sp_str);
      fprintf(stderr,"Starting PC is : %s  base 10:  %6d\nStarting SP is : %s  base 10:  %6d\n\n",pc_str,btoi(pc_str),sp_str,btoi(sp_str));
   }
   strcpy (ZeroRegister,   "0000000000000000") ;
   strcpy (PositiveOne,    "0000000000000001") ;
   strcpy (NegativeOne,    "1111111111111111") ;
   strcpy (Amask,          "0000111111111111") ;
   strcpy (Smask,          "0000000011111111") ;

}			/* END InitializePCandStackPointer */
