#ifndef TECPLOTREADER_H
#define TECPLOTREADER_H
#include <iostream>
#include <fstream>
#include <string>
//VTK includes
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkStructuredGrid.h"
#include "vtkUnstructuredGridWriter.h"
#include "readWriteVtkThings.h"
#include "vtkCellType.h"
#include "vtkFloatArray.h"
#include "converter.h"
class tecplotReader
{
   private:
         //char* inputTecplotFile;
         //char* outputVTKFile;
         ifstream fileI;      //input files
         int nX;              //number of x coordinated
         int nY;              //number of y coordinates
         vtkUnstructuredGrid* uGrid;
         vtkPoints* pts;
         vtkFloatArray** parameterData;
         double array [ 6 ];   //there are 6 kinds of data in the INEL tecplot files
         double* x;
         double* y;
         double* u;
         double* v;
         double* w;
         double* measurement;
         double* absVel;
         int numCells;
         int numVertices;
   
   public:
         tecplotReader();
         ~tecplotReader();
         /*vtkUnstructuredGrid**/ void tecplotToVTK( char* inFileName, char* outFileName, int debug, int nx, int ny );
};
#endif //TECPLOTREADER_H
