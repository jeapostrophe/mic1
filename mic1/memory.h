#define  MemoryChipSize 1024
typedef  DataBusType Memory_Chip[MemoryChipSize] ;

extern Memory_Chip MemoryChip3;

void InitializeMemory (const char *program_file);
void ActivateMemory (AddressBusType Address, DataBusType Data, Bit ReadBit, Bit WriteBit);
