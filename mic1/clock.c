#include "clock.h"


struct Clock Quartz = {0, 0} ;      /* Quartz is of TYPE clock */

char FirstSubcycle() {
  return (Quartz.Subcycle == 1);
}

char SecondSubcycle() {
  return (Quartz.Subcycle == 2);
}

char ThirdSubcycle() {
  return (Quartz.Subcycle == 3);
}

char FourthSubcycle() {
  return (Quartz.Subcycle == 4);
}

int Cycle () {
  return (Quartz.Cycle);
}

int Subcycle () {
  return (Quartz.Subcycle) ;
}

void GeneratePulse () {
  Quartz.Subcycle = (Quartz.Subcycle % 4) + 1 ;
  if (Quartz.Subcycle == 1)
    Quartz.Cycle = Quartz.Cycle + 1 ;
}
