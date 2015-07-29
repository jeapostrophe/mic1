#define TRUE  1 
#define FALSE 0

struct Clock
      {
         int Cycle ;
         int Subcycle ;		/* 0..4 */
      } ;

struct Clock Quartz = {0, 0} ;		/* Quartz is of TYPE clock */

FirstSubcycle () 
{

    if (Quartz.Subcycle == 1) 
            return TRUE ;
       else return FALSE ;

}			/* END FirstSubcycle */

SecondSubcycle () 
{

    if (Quartz.Subcycle == 2) 
            return TRUE ;
       else return FALSE ;

}			/* END SecondSubcycle */

ThirdSubcycle () 
{

    if (Quartz.Subcycle == 3) 
            return TRUE ;
       else return FALSE ;

}			/* END ThirdSubcycle */

FourthSubcycle () 
{

    if (Quartz.Subcycle == 4) 
            return TRUE ;
       else return FALSE ;

}			/* END FourthSubcycle */

int Cycle ()
{

   return (Quartz.Cycle) ;

}

void GeneratePulse ()
{

    Quartz.Subcycle = (Quartz.Subcycle % 4) + 1 ;
    if (Quartz.Subcycle == 1)
        Quartz.Cycle = Quartz.Cycle + 1 ;

}			/* END GeneratePulse */
