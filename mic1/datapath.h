void InitializePCandStackPointer (int pc, int sp);
void DumpRegisters ();
void ActivateDataPath (AddressBusType MarRegs, DataBusType MbrRegs,
                       DataBusType ABits, DataBusType BBits, DataBusType CBits,   
                       Bit AmuxBit, TwoBits AluBits, TwoBits ShiftBits,
                       Bit MbrBit, Bit MarBit, Bit *NBit, Bit *ZBit);
