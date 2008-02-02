#ifndef INLETOUTLET_H
#define INLETOUTLET_H

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <vector>
#include <cstdio>
#include <time.h>
#include <math.h>
class Vertex{
   public:
      float x;
      float y;
      float z;
      int vertexNumber;
      Vertex(void);
      ~Vertex(void);
      Vertex(int vertexNumber, float x, float y, float z);
};

class Cell{
   public:
      typedef  std::vector<Vertex>  Vertices;
      Vertices neighbors; 
      int cellNumber;
      Cell(void);
      ~Cell(void);
      Cell( int cellNumber );
};

class InletOutlet{
  public:    
      typedef  std::vector<Cell>  Cells; 
		Cells top;
		Cells bottom;
      Cells surface;
		Cells possibilities;
      Cells usable; 
      int height;
      int width;
      int depth;
      int startPosition; //Vertex number of lower left corner of inlet/outlet
      int x_vert_ct;     // number of verts in the x-direction
      int y_vert_ct;     // number of verts in the y-direction
      int maxLth;        // maximum allowable baffle size
      int minLth;        // minimum allowable baffle size
      int minHgt;        // minimum allowable baffle size
      int x_move;        // Shifts Vertex# to next value in x direction
      int y_move;        // Shifts Vertex# to next value in y direction
      int y_dist;
      int x_dist;
      int vertexStart;
      int cellStart;
      int distanceMax;
      int i_offset;
      int j_offset;
      int topCellNumber;  
      int topVertexNumber;
      int numCellsInSurface;
	   int range;
      
      InletOutlet( InletOutlet *);
      InletOutlet( int ,int , int, int, int, int ,int , int, int, int);
      ~InletOutlet(void);

      void readSurfaceFile( void );
      void findNeighbors( void );
      void RandDef( void );
      void findTopCells( void );
      void findBottomCells( void );
      void InitializePossiblities( void );             
      void setParams( void ); 
      void evolveInletOutlet( void );
	  int Rand( void );
};
#endif
