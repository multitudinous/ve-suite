#ifndef CREATE_STOVE_H
#define CREATE_STOVE_H

#include "H/stove.h"
#include  <vector>

using namespace std;

class Create_stove{
public:
   
   Create_stove();
   ~Create_stove();

   void RunNewStove(int,vector<double>,vector<double>,vector<double>,
                     vector<double>,vector<double>,vector<double>,vector<double>,int);

   Stove *stove;

   int numBaffInts;
   int numBaffs;
   int *baff;
   int inlet;
   int outlet;

   double n_fit_new;
   double sigma;
   double fitness;
   double area;

};

#endif
