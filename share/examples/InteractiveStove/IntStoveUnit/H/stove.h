#ifndef STOVE_H
#define STOVE_H

#include <iostream>
#include <sstream>
//#include <iomanip>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <math.h>
// #include <tk.h>

#include "baffle.h"
#include "inlet.h"
#include <vector>
//#include "../../GA/H/randomlib.h"
//#include "../../Control/Control.h"


class Stove{
   public:
      typedef std::vector<Baffle*> BaffleList;
    
      static char filename[30]; // name of file for Star-CD to read
      static int  cell_type;    // Star-CD cell table number for baffles to be created
      static int  last_value;   // last value in cwrite files, I don't yet know what it means
      static int  last_cell;    // cell number of last cell in the Star-CD file
       	 
      int    baffCount;
      int    numOperators;
      int    numCells;
      int    in_out;
      int    ppid;
      double fitness1;
      double fitness2;

      InletOutlet *inlet;
      InletOutlet *outlet;
    
	   typedef std::vector<double> DoubleList;
	   DoubleList fhist;
	

     // Stove( Control *CNTRL );    
      Stove( int baffCount0, int *params, int in, int out );
	   ~Stove();

	   double Distance( Stove *s_compare );
      void baffleWrite();
      void stoveEval();
      void mutateNewBaff();
      void mutateBaff();
      bool checkBaffles();
      void writeStove(FILE *);
      void fitnessArea(int , double, double *);
      void fitnessVarience(int ,double, double *);
      void Copy(Stove *);
	   void Copy(Stove *, int, int);
      void CalcFitness(double);
	   void GetCrossPoints( int &crosspt );
	   void CrossoverK1( Stove *, Stove *, int );
	   void CrossoverK2( Stove *, Stove *, int );
	   void GetFitness( int );
	   void KillStarCD( void );
	   void CompileAndExecu( int );
	   void CheckForFitness( int );
	   void GetDesignSpace( DoubleList designSpace );
      void writeBoundaryFile( void );
      void mutateInletOutlet( InletOutlet *stuff );
      void copyInletOutlet( InletOutlet *copy, InletOutlet *x );
      void findUsableCells( void );
      void BuildBaffSTL( int );
	   static void *ExecuStarCD ( void * );	
      // Routines for sending info to "fitness.dat"
      BaffleList bafflelist;
	  bool doubleBaffle( void );

	  int Rand( void );
};

#endif
