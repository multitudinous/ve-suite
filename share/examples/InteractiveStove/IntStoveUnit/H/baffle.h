#ifndef BAFFLE_H
#define BAFFLE_H

#include <iostream>
#include <iomanip>
#include <fstream>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
//#include "../../GA/H/randomlib.h"

class Baffle{
  public:    
    int  x_cell_ct;       // number of cells in the x-direction
    int  y_cell_ct;	  // number of cells in the y-direction
    int  z_cell_ct;	  // number of cells in the z-direction
    int  maxyLth;  	  // maximum allowable baffle size
    int  maxxLth;  	  // maximum allowable baffle size
    int  minLth;  	  // minimum allowable baffle size
    int  minHgt;  	  
    int  x_move;	  //Shifts Vertex# to next value in x direction
    int  y_move;	  //Shifts Vertex# to next value in y direction
    int  z_move;	  //Shifts Vertex# to next value in z direction
			  
    
    int  vertex[8];
    int* baff;
    int  start_vertex;
    
  Baffle();
  Baffle(int params[],int i);
  ~Baffle(void);
  void setStoveParams();
  void findFirst();             
  void convertParams(int params[],int baffNum);
  void defineVerts(int* &baff);
  void RandDef(int* &baff);
  bool intoWall( void );
  void alongWall( void ); 
  void chopBaff(int* &baff,int chopVal); 
  void makeNull(int* &baff);
  void writeBaffle(FILE *log);
  int Rand( void );
};

#endif
