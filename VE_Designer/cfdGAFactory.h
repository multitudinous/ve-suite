#ifndef CFD_GAFACTORY_H
#define CFD_GAFACTORY_H

#include "cfdGABase.h"
#include "cfdVeReadParamFactory.h"
#include "cfdVeReadParam.h"

class cfdGAFactory 
{
   public:
      cfdGAFactory(char*);
      ~cfdGAFactory();

      void initializeFromFile(int );
      void initializeFromComputer(int );
      void initializeFromMixed(int );
      
      
   public:
      cfdVeReadParam *param_file;
      std::vector <cfdGABase *> ga;
      int numparamspergene;
      double *galowerlimit;
      double *gaupperlimit;
      char* gaparamsfilename;
      char* gaboundaryfilename;
      
   public:
      double total_fitness;
      int initialtype;
      int popsize;
      
};

#endif
