#include "mic1.h"
#include "control.h"
#include "datapath.h"

Bit         NBit, ZBit, AmuxBit, MbrBit, MarBit ;
DataBusType ABits, BBits, CBits ;
TwoBits     AluBits, ShiftBits ;        


void ActivateCpu (AddressBusType MarRegs, DataBusType MbrRegs, Bit *ReadBit, Bit *WriteBit) {

    ActivateControlStore (NBit, ZBit, ABits, BBits, CBits, 
                          &AmuxBit, AluBits, ShiftBits, &MbrBit, &MarBit,
                          ReadBit, WriteBit) ;

    ActivateDataPath (MarRegs, MbrRegs, ABits, BBits, CBits,
                      AmuxBit, AluBits, ShiftBits, MbrBit, MarBit,
                      &NBit, &ZBit) ;

}			/* END ActivateCpu */
