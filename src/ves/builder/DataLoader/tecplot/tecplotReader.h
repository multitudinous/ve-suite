/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef TECPLOTREADER_H
#define TECPLOTREADER_H

#include <iostream>
#include "MASTER.h"
#include "GLOBAL.h"
#include "Manager.h"

class vtkUnstructuredGrid;
class vtkFloatArray;
class vtkPoints;

namespace ves
{
namespace builder
{
namespace DataLoader
{
class tecplotReader
{
public:
    tecplotReader( std::string );

    ~tecplotReader();

    //vtkUnstructuredGrid * GetUGrid();

    int GetNumberOfOutputFiles();
    
    vtkUnstructuredGrid * GetOutputFile( int );

private:
    std::string inputFileNameAndPath;
    vtkUnstructuredGrid * ugrid;
    int numberOfOutputFiles;
    int tecplotIsStarted;
    tecplot::sdk::integration::Manager* manager;
    EntIndex_t numZones;
    EntIndex_t connectivityShareCount;
    EntIndex_t numVars;
    int dimension;  //determine whether original tecplot data uses 1d, 2d, or 3d coordinates
    int xIndex;
    int yIndex;
    int zIndex;
    VarName_t * varName;
    int numParameterArrays;
    int coordDataSharedAcrossZones;
    int totalNumberOfElements;
    int totalNumberOfNodalPoints;
    int ii;
    int nodeOffset;
    int elementOffset;
    vtkPoints *vertex;
    vtkFloatArray ** parameterData;

    void OneTimeSetup();

    void OneTimeCleanup();

    std::string getExtension( const std::string& s );

    int isFileReadable( const std::string filename );

    void readVariable( EntIndex_t currentZone, int varNumber, char * varName, vtkFloatArray *& parameterData );

    vtkFloatArray * zeroArray( std::string varName, int numTuples );

    void readVectorNameAndUpdateIndex( int currentIndex, int currentVar, std::string s, std::string & vecName, int * vectorIndex );

    void processAnyVectorData( int numNodalPointsInZone, vtkFloatArray ** parameterData );

    void computeNumberOfOutputFiles();

    void computeDimension();

    void seeIfDataSharedAcrossZones();

    void processZone( EntIndex_t currentZone );
};
}
}
}
#endif

