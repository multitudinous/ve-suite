#ifndef CFD_GAMANAGER_H
#define CFD_GAMANAGER_H

#include "cfdVeReadParamFactory.h"
#include "cfdVeReadParam.h"
#include "cfdCalculator.h"
#include <list.h>
#include <fstream>
#include "cfdGABase.h"
#include "cfdGAFactory.h"

class cfdGAManager : public cfdGAFactory
{
   public:
      
   cfdGAManager(char*);
   ~cfdGAManager();

   void selector(int type);
   void replacer(int type);
   void crossover(int p1, int p2, int c1, int c2);
   void mutator(int c1);
   void breed();
   //void report(ostream &stats,int g);
   //void save_final(int r); 
   void rankSelector();
   int rouletteWheelSelector();
   void tournamentSelector();
   void uniformSelector();

   int randomReplaceSelector();
   void randomReplace(int p1,int p2,int c1,int c2, int mut1, int mut2);
   void rouletteWheelReplace();
   void randomEliteReplace();

   void findTotalFitness();
   double getTotalFitness();
   //void max_min();
   void full_sort();
   
   private:
   //datebase modified flag
   int modify_flag;  
   int popsize;
   int numParams;
   int run;
   double total_fitness;
   double max1,max2;


};

#endif
