/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

class vtkDataObject;
class vtkMultiBlockDataSet;
class vtkUnstructuredGrid;
class vtkDoubleArray;
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
    tecplotReader( std::string inputFileNameAndPath );

    ~tecplotReader();

    void SetMultiBlockOn();

    int GetNumberOfTimesteps();

    vtkDataObject* GetOutput( const int timestep );

private:
    std::string inputFileNameAndPath;
    bool multiblockOutput;
    vtkMultiBlockDataSet* multiblock;
    vtkUnstructuredGrid* ugrid;
    int numberOfTimesteps;
    ///Defines the total number of zones in a tecplot files. For example, if the
    ///tecplot file has 9 zones and 5 timesteps numZones will be 45. The number
    ///of zones will be all inclusive for all timesteps and all zones.
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
    int nodeOffset;
    int elementOffset;
    vtkPoints* vertex;
    vtkDoubleArray** parameterData;
    int * timeToInitVtk;
    int * numZonesAtTimestep;
    LgIndex_t IMax, JMax, KMax;
    
    ///???
    void ReadVariable( const EntIndex_t currentZone, int varNumber, const char* varName, vtkDoubleArray* scalarData );
    ///???
    vtkDoubleArray* ZeroArray( std::string varName, int numTuples );
    ///???
    void ProcessAnyVectorData( vtkDoubleArray** vectorData );
    ///???
    void ComputeNumberOfTimesteps();
    ///???
    void ComputeDimension();
    ///???
    void SeeIfDataSharedAcrossZones();
    ///???
    void InitializeVtkData();
    ///???
    void ReadElementInfoInZone( const EntIndex_t currentZone, ZoneType_e & zoneType, LgIndex_t & numElementsInZone,
                                int & numNodesPerElement, int & numFacesPerCell, int & numNodalPointsInZone );
    ///???
    void ReadZoneName( const EntIndex_t currentZone );
    ///???
    void AddCellsToGrid( const EntIndex_t currentZone, const ZoneType_e zoneType, const LgIndex_t numElementsInZone,
                         const int numNodesPerElement );
    ///???
    void AddOrderedCellsToGrid( const LgIndex_t numElementsInZone, const int numNodesPerElement );
    ///???
    void AddFaceCellsToGrid( const EntIndex_t currentZone, const ZoneType_e zoneType, const LgIndex_t numElementsInZone );
    ///???
    void ReadNodalCoordinates( const EntIndex_t currentZone, const int numNodalPointsInZone );

    void ReadNodeAndCellData( const EntIndex_t currentZone, const LgIndex_t numElementsInZone,
                              const int numNodalPointsInZone, int parNum );
    ///???
    void AttachPointsAndDataToGrid();
    
    void CountNumberOfTimestepsUsingSolnTime();
    ///???
    int GetStartingZoneForTimestep( const int timestep );
    ///???
    int GetEndingZoneForTimestep( const int timestep );
    ///Test variable index 3 for the Z coord array
    bool TestForZVariable();
    ///???
    EntIndex_t GetNumZonesInCurrentFile( const EntIndex_t startZone );
};
}
}
}
#endif

