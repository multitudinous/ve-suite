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
#include <ves/builder/DataLoader/tecplot/Manager.h>

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

    int GetNumberOfOutputFiles();

    vtkUnstructuredGrid* GetOutputFile( const int i );

private:
    std::string inputFileNameAndPath;
    vtkUnstructuredGrid* ugrid;
    int numberOfOutputFiles;
    EntIndex_t numZones;
    EntIndex_t connectivityShareCount;
    EntIndex_t numVars;
    int dimension;  //determine whether original tecplot data uses 1d, 2d, or 3d coordinates
    int xIndex;
    int yIndex;
    int zIndex;
    VarName_t* m_varName;
    int numParameterArrays;
    int coordDataSharedAcrossZones;
    int totalNumberOfElements;
    int totalNumberOfNodalPoints;
    int ii;
    int nodeOffset;
    int elementOffset;
    vtkPoints* vertex;
    vtkFloatArray** parameterData;
    int * timeToInitVtk;

    std::string getExtension( const std::string& s );

    int isFileReadable( const std::string filename );

    void ReadVariable( EntIndex_t currentZone, int varNumber, const char* varName, vtkFloatArray* scalarData );

    vtkFloatArray* ZeroArray( std::string varName, int numTuples );

    void ReadVectorNameAndUpdateIndex( int currentIndex, int currentVar, std::string s, std::string& vecName, int* vectorIndex );

    void ProcessAnyVectorData( int numNodalPointsInZone, vtkFloatArray** vectorData );

    void ComputeNumberOfOutputFiles();

    void ComputeDimension();

    void SeeIfDataSharedAcrossZones();

    void InitializeVtkData( EntIndex_t currentZone );

    void ReadElementInfoInZone( EntIndex_t currentZone, ZoneType_e & zoneType, LgIndex_t & numElementsInZone,
                                int & numNodesPerElement, int & numFacesPerCell, int & numNodalPointsInZone );

    void ReadZoneName( EntIndex_t currentZone );

    void AddCellsToGrid( const EntIndex_t currentZone, const ZoneType_e zoneType, const LgIndex_t numElementsInZone,
                         const int numNodesPerElement, const int numNodalPointsInZone );

    void ReadNodalCoordinates( const EntIndex_t currentZone, const int numNodalPointsInZone );

    void ReadNodeAndCellData( const EntIndex_t currentZone, const LgIndex_t numElementsInZone, const int numNodalPointsInZone );

    void AttachPointsAndDataToGrid( const int numNodalPointsInZone );
    
    void CountNumberOfFilesUsingSolnTime();

    int GetStartingZoneForFile( const int fileNum );

    int * GetVtkInitArray();
};
}
}
}
#endif

