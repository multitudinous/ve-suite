
/*
   DESCRIPTION:
   The base GABase class just defines the genome interface - how to mutate,
   crossover, evaluate, etc.  Use the data type to store the information 
   and use the genome part to tell the GA how it should operate on the data. 
*/

#ifndef CFD_GABASE_H
#define CFD_GABASE_H

#include "cfdCalculator.h"

class cfdGABase
{
   public:
   //The mutator makes a change to the genome with likeliehood determined by the mutation rate parameter
   int * mutator();  
  
   double GaussianMutator();
   //The crossover member function is used to set the default mating mode for the genomes 
   void crossover();

   //Set the genome's objective function.  
   //This also sets marks the evaluated  flag to indicate that the genome must be re-evaluated.
   void evolve();
   
   
   // This method is used to determine how similar two genomes are. 
   void comparator();

   //
   void findfit();

   void designEvaluator(float *gaPamas);

   public:
   cfdGABase(int n);
   ~cfdGABase();

   public:
   double score;			// value returned by the objective function
   double fitness;		// (possibly scaled) fitness score
   bool evaluated;		// has this genome been evaluated?
   unsigned int neval;		// how many evaluations since initialization?
   double eval;		// objective function
   bool evd;		// evaluation data (specific to each genome)
   double mutr;			// the mutation operator to use for mutations
   bool init;		// how to initialize this genome
   bool cmp;		// how to compare two genomes of this type
   int  numMutr;
   int nParams ;
   float *gaParams;

   cfdCalculator* mCalculator;
   

};
#endif
