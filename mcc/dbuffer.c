/*	Programmer : Richard Boccuzzi	*/
/*	Filename   : dbuffer.c			*/

#include <string.h>
#include <sys/file.h>
#include <unistd.h>

#include "mcc.h"
#include "dbuffer.h"
#include "symtab.h"
#include "errors.h"

int linenum = 0;

/*----------------------------------------------------------------------*/

#define READSIZE 1025
#define BUFSIZE (2*(READSIZE+1))
static char buf[BUFSIZE];
static int beginlptr = 0;
static int endlptr = 0;
static int forwardptr = 0;
static int lexemeptr = 0;
static int fd;
int eof= 0;

/*----------------------------------------------------------------------*/

void initbuf(char *file)
/* opens up file, fills up the buffer, puts the sentinels in place,
	and returns the file descriptor	*/
{
	void moveupline();
	char *getlinex();

	if(file == NULL){
          fd = 0;
	}else{
	  fd = open(file, O_RDONLY);
	}
	if (fd >= 0)
	{
		int i;
		buf[READSIZE] = EOF;
		buf[BUFSIZE-1] = EOF;
		if ((i = read(fd, buf, READSIZE))!=READSIZE)
			buf[i] = EOF;
		moveupline();
		if ((debuglevel & scanbug_m) && (linenum >=0) && !eof)
			fprintf(stderr,"%d:%s",linenum,getlinex());
	}
	else
	{
		printf("Error, could not open %s for reading\n",file);
		exit(1);
	}
} /* end initbuf() */

/*----------------------------------------------------------------------*/

int getcurpos()
/* return the offset of the current position from the begining of the line */
{
	int pos;
	if (beginlptr < forwardptr)
		if ((forwardptr < READSIZE) || (beginlptr > READSIZE))
			pos = forwardptr - beginlptr;
		else 
			pos = (forwardptr - beginlptr) - 1;
	else
		pos = BUFSIZE - (beginlptr - endlptr);
	return(pos);
} 	/* end of getcurpos */

/*----------------------------------------------------------------------*/

char *getlinex()
/* return the line between bbeginlptr and ebeginlptr */
{
	char *str,*s;
	int len;
	if (beginlptr < endlptr)
	{
		if ((endlptr < READSIZE) || (beginlptr > READSIZE))
		{
			len = endlptr - beginlptr;
			str = (char *) salloc(len+1);
			strncpy(str,buf+beginlptr, len);
			str[len]='\0';
		}
		else 
		{
			len = (endlptr - beginlptr) - 1;
			str = (char *) salloc(len+1);
			strncpy(str,buf+beginlptr,READSIZE - beginlptr);
			strncat(str,buf+(READSIZE+1),endlptr - (READSIZE+1));
			str[len]='\0';
		} /* end if */
	}
	else
	{
		len = BUFSIZE - (beginlptr - endlptr);
		str = (char *) salloc(len+1);
		strncpy(str,buf+beginlptr, (BUFSIZE-1) - beginlptr);
		strncat(str,buf, endlptr);
		str[len]='\0';
	} /* end if */
	for(s = str; *s; s++)
		if (*s == '\t')
			*s = ' ';
	return(str);
} /* end getlinex() */

/*----------------------------------------------------------------------*/

void moveupline()
{
	int i;
	char ch;
	int oldptr = beginlptr;
	beginlptr = endlptr;

	do
	{
		if (buf[endlptr] == EOF )
		{
			if (endlptr==READSIZE)
			{
              if ((i = read(fd, buf+(READSIZE+1), READSIZE)) != READSIZE)
                buf[(READSIZE+1)+i] = EOF;
				++endlptr;
			}
			else if (endlptr == (BUFSIZE-1))
			{
				if ((i = read(fd, buf, READSIZE)) != READSIZE)
					buf[i]=EOF;
				endlptr = 0;
			}
			else
			{
				eof = 1;
				linenum--;
				beginlptr = oldptr;
				break;
			}
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
	if (buf[forwardptr] == EOF)
		switch(forwardptr)
		{
        case (BUFSIZE-1):	forwardptr = -1;
			case READSIZE:	forwardptr++;
			default:	break;
		}
	return(ch);
} /* end getbc() */

/*----------------------------------------------------------------------*/

void ungetbc()
/* moves the forward pointer back to the last character pointed to */
{
	switch(--forwardptr)
	{
    case -1:	forwardptr = (BUFSIZE-1);
		case READSIZE:	--forwardptr;
	}
} /* end ungetbc() */

/*----------------------------------------------------------------------*/

char *getlex()
/* return the lexeme pointed to by lexemeptr, advances ptr to where 
	forwardptr is */
{
	char *str;
	int len;
	if (lexemeptr < forwardptr)
	{
		if ((forwardptr < READSIZE) || (lexemeptr > READSIZE))
		{
			len = forwardptr - lexemeptr;
			str = (char *) salloc(len+1);
			strncpy(str,buf+lexemeptr, len);
			str[len]='\0';
		}
		else 
		{
			len = (forwardptr - lexemeptr) - 1;
			str = (char *) salloc(len+1);
			strncpy(str,buf+lexemeptr,READSIZE - lexemeptr);
			strncat(str,buf+(READSIZE+1),forwardptr - (READSIZE+1));
			str[len]='\0';
		} /* end if */
	}
	else
	{
		len = BUFSIZE - (lexemeptr - forwardptr);
		str = (char *) salloc(len+1);
		strncpy(str,buf+lexemeptr, (BUFSIZE-1) - lexemeptr);
		strncat(str,buf, forwardptr);
		str[len]='\0';
	} /* end if */
	lexemeptr = forwardptr;
	return(str);
} /* end getlex() */

/*----------------------------------------------------------------------*/

void alignptrs()
{
	lexemeptr = forwardptr;
}

/*----------------------------------------------------------------------*/
