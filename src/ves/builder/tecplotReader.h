/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
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
    size_t I_Lower;  //lower location for the header
    size_t I_Upper;  //another upper location for the header
    std::vector< std::string > variablNames;  //a vector of strings to store variabl names
    std::vector< int >::iterator vectorIntIterator;     //an iterator for vectors of ints
    std::vector< std::string >::iterator vectorStringIterator; //an iterator for vectors of strings

public:
    tecplotReader();
    ~tecplotReader();
    vtkUnstructuredGrid* tecplotToVTK( std::string inFileName, int debug );
};
#endif //TECPLOTREADER_H
