#ifndef TECPLOTREADER_H
#define TECPLOTREADER_H
#include <math.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
//VTK includes
class vtkPoints;
class vtkUnstructuredGrid;
class vtkStructuredGrid;
class vtkUnstructuredGridWriter;

class vtkCellType;
class vtkCellType;
class vtkFloatArray;


class tecplotReader
{
   private:
         std::ifstream fileI; //input files
         int nX;              //number of x coordinated
         int nY;              //number of y coordinates
         int colsOfData;      //number of columns of data
         int numOfParameters;   //number of parameters 
         vtkUnstructuredGrid* uGrid;
         vtkPoints* pts;
         vtkFloatArray** parameterData;
         double* array;
         int numCells;
         int numVertices;
         std::vector< int > locationQuotMarks;  //stores the locations of the quotation marks
         //allocate memory once we know the file exists and nX and nY are known
         void allocateVariables();
         std::string header;    //store the header string in this string var
         std::string tempString; //temporary storage for strings
         std::string::iterator I;   //an iterator to parse through strings
         unsigned int I_Lower;  //lower location for the header
         unsigned int I_Upper;  //another upper location for the header         
         std::vector< std::string > variablNames;  //a vector of strings to store variabl names
         std::vector< int >::iterator vectorIntIterator;     //an iterator for vectors of ints
         std::vector< std::string >::iterator vectorStringIterator; //an iterator for vectors of strings 
            
   public:
         tecplotReader();
         ~tecplotReader();
         vtkUnstructuredGrid* tecplotToVTK( char* inFileName, int debug );
};
#endif //TECPLOTREADER_H
