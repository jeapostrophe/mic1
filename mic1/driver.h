void Set_blocking_io();
void Set_nonblocking_io();

extern int  polled_io;
extern char input_char;

int True_ascii_to_mem_ascii(char *mem_location, const char *character);
