/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : dbuffer.c			*/

#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#include "mcc.h"
#include "dbuffer.h"
#include "symtab.h"
#include "errors.h"

int linenum = 0;

/*----------------------------------------------------------------------*/

static char *buf;
static int beginlptr = 0;
static int endlptr = 0;
static int forwardptr = 0;
static int lexemeptr = 0;
int eof= 0;

/*----------------------------------------------------------------------*/

void initbuf(char *file)
/* opens up file, fills up the buffer, puts the sentinels in place,
	and returns the file descriptor	*/
{
	void moveupline();
	char *getlinex();

	if (file == NULL) {
      int fd = 0;
      int total_size = 0;
      int buf_size = 2;
      buf = malloc(buf_size);
      const int READSIZE = 1024;
      char localbuf[READSIZE+1];
      
      while (1) {
        int rd = read(fd, localbuf, READSIZE);
        int next_total_size = total_size + rd;
        int next_buf_size = next_total_size + 2;
        char *next_buf = malloc(next_buf_size);
        if (next_buf == NULL) {
          printf("Error, could not allocate buffer for stdin\n");
          exit(1);
        }
        
        memcpy(next_buf, buf, total_size);
        memcpy(next_buf + total_size, localbuf, rd);
        next_buf[next_total_size] = EOF;
        next_buf[next_total_size+1] = 0;
        free(buf);
        
        buf = next_buf;
        total_size = next_total_size;
        buf_size = next_buf_size;
        
        if (rd < READSIZE) {
          break;
        }
      }
	} else {
	  int fd = open(file, O_RDONLY);
      if (fd < 0) {
        printf("Error, could not open %s for reading\n",file);
		exit(1);
      }
      struct stat st;
      fstat(fd, &st);
      int size = st.st_size;
      buf = malloc(size+1);
      if (buf == NULL) {
        printf("Error, could not allocate buffer for %s\n",file);
		exit(1);
      }
      int rd = read(fd, buf, size);
      buf[rd] = 0;      
	}
    
    moveupline();
    if ((debuglevel & scanbug_m) && (linenum >=0) && !eof)
      fprintf(stderr,"%d:%s",linenum,getlinex());

} /* end initbuf() */

/*----------------------------------------------------------------------*/

int getcurpos()
/* return the offset of the current position from the begining of the line */
{
	int pos;
    pos = (forwardptr - beginlptr) - 1;
	return(pos);
} 	/* end of getcurpos */

/*----------------------------------------------------------------------*/

char *getlinex()
/* return the line between bbeginlptr and ebeginlptr */
{
	char *str,*s;
	int len;
    len = endlptr - beginlptr;
    str = (char *) salloc(len+1);
    strncpy(str,buf+beginlptr, len);
    str[len]='\0';
	for(s = str; *s; s++)
		if (*s == '\t')
			*s = ' ';
	return(str);
} /* end getlinex() */

/*----------------------------------------------------------------------*/

void moveupline()
{
	char ch;
	int oldptr = beginlptr;
	beginlptr = endlptr;

	do {
      if (buf[endlptr] == EOF ) {
        eof = 1;
        linenum--;
        beginlptr = oldptr;
        break;
      }
	} while ((ch = buf[endlptr++]) != '\n');
	linenum++;
}

/*----------------------------------------------------------------------*/

char getbc()
/* this proc. moves forwardptr along the buffer, returning either
	the char it is pointing to */
{
	char ch = buf[forwardptr++];
	return(ch);
} /* end getbc() */

/*----------------------------------------------------------------------*/

void ungetbc()
/* moves the forward pointer back to the last character pointed to */
{
  --forwardptr;
} /* end ungetbc() */

/*----------------------------------------------------------------------*/

char *getlex()
/* return the lexeme pointed to by lexemeptr, advances ptr to where 
	forwardptr is */
{
	char *str;
	int len;

    len = forwardptr - lexemeptr;
    str = (char *) salloc(len+1);
    strncpy(str,buf+lexemeptr, len);
    str[len]='\0';
	    
	lexemeptr = forwardptr;
	return(str);
} /* end getlex() */

/*----------------------------------------------------------------------*/

void alignptrs()
{
	lexemeptr = forwardptr;
}

/*----------------------------------------------------------------------*/
