#include <strings.h>
#include "globals.h"

Add (Left, Right, AluOutput) 
DataBusType Left, Right ;
DataBusType AluOutput ;

{

   Bit Carry ;
   int I ;

        Carry = Zero ;

        for (I = DataWordSize-1 ; I >= 0 ; I--)

        if ((Carry == Zero) && (Left[I] == Zero) && (Right[I] == Zero))
           {
	      AluOutput[I] = Zero ;  
	      Carry = Zero ;
           }
          else if ((Carry == Zero) && (Left[I] == Zero) && (Right[I] == One))
           {
              AluOutput[I] = One ;  
	      Carry = Zero ;
           }
          else if ((Carry == Zero) && (Left[I] == One) && (Right[I] == Zero))
	   {
              AluOutput[I] = One ;  
	      Carry = Zero ;
	   }
          else if ((Carry == Zero) && (Left[I] == One) && (Right[I] == One))
	   {
              AluOutput[I] = Zero ;  
	      Carry = One ;
	   }
          else if ((Carry == One) && (Left[I] == Zero) && (Right[I] == Zero))
	   {
              AluOutput[I] = One ;  
	      Carry = Zero ;
	   }
          else if ((Carry == One) && (Left[I] == Zero) && (Right[I] == One))
	   {
              AluOutput[I] = Zero ;  
	      Carry = One ;
	   }
          else if ((Carry == One) && (Left[I] == One) && (Right[I] == Zero))
	   {
              AluOutput[I] = Zero ;  
	      Carry = One ;
	   }
          else if ((Carry == One) && (Left[I] == One) && (Right[I] == One))
	   {
              AluOutput[I] = One ;  
	      Carry = One ; 
	   }

}				/* END Add */

And (LeftOperand, RightOperand, AluOutput)
DataBusType LeftOperand, RightOperand ; 
DataBusType AluOutput ;

{
        int I ;

        for (I = 0 ; I < DataWordSize-1 ; I++)

            if ((LeftOperand[I] == One) && (RightOperand[I] == One))
                 AluOutput[I] = One ; 
            else AluOutput[I] = Zero ; 

}				/* END And */

NotA (LeftOperand, AluOutput)
DataBusType  LeftOperand ;
DataBusType  AluOutput ;

{
       int I ;

       for (I = 0 ; I < DataWordSize-1 ; I++)

              if (LeftOperand[I] == One)  
	           AluOutput[I] = Zero ; 
              else AluOutput[I] = One ;

}				/* END NotA */

ActivateAlu (LeftOperand, RightOperand, AluBits,
             AluOutput, NBit, ZBit) 

DataBusType LeftOperand, RightOperand ;
TwoBits     AluBits ;
DataBusType AluOutput ; 
Bit         *NBit ;
Bit         *ZBit ;

{
       int I ;
       Bit AluBit0 ;
       Bit AluBit1 ;

       AluBit0 = AluBits[0] ;
       AluBit1 = AluBits[1] ;

       if ((AluBit0 == Zero) && (AluBit1 == Zero)) 
           Add (LeftOperand, RightOperand, AluOutput) ;

       else if ((AluBit0 == Zero) && (AluBit1 == One)) 
           And (LeftOperand, RightOperand, AluOutput) ;

       else if ((AluBit0 == One) && (AluBit1 == Zero)) 
           strcpy (AluOutput, LeftOperand) ;

       else if ((AluBit0 == One) && (AluBit1 == One))  
           NotA (LeftOperand, AluOutput) ;  

       if (AluOutput[0] == '1') 
	    *NBit = '1' ;
       else *NBit = '0' ;

       *ZBit = '1' ;

       for (I = 0 ; I < DataWordSize-1 ; I++)
           if (AluOutput[I] == '1')
	      *ZBit = '0' ;

}				/* END ActivateAlu */
