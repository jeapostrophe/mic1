struct Clock {
  int Cycle;
  int Subcycle;        /* 0..4 */
};

char FirstSubcycle();
char SecondSubcycle();
char ThirdSubcycle();
char FourthSubcycle();

int Cycle();
void GeneratePulse();
