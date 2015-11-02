extern int MicroPc;

void ActivateControlStore (Bit NBit, Bit ZBit,
                           DataBusType ABits, DataBusType BBits, DataBusType CBits,
                           Bit *AmuxBit, TwoBits AluBits, TwoBits ShiftBits, 
                           Bit *MbrBit, Bit *MarBit, Bit *ReadBit, Bit *WriteBit);
void BurnInProm (const char *prom_file);
