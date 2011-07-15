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
#include <ves/builder/DataLoader/tecplot/tecplotReader.h>

#include "TecIntegrationManager.h"
#include "StringList.h"

#include <vtkMultiBlockDataSet.h>
#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkIdList.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/filesystem/operations.hpp>

#include <fstream>
#include <iostream>

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace ves::builder::DataLoader;

tecplotReader::tecplotReader( std::string inputFileNameAndPath )
{
    this->inputFileNameAndPath = inputFileNameAndPath;
    this->multiblockOutput = false;
    this->multiblock = NULL;
    this->ugrid = NULL;
    this->numberOfTimesteps = 0;
    this->numZones = 0;
    this->connectivityShareCount = 0;
    this->numVars = 0;
    this->dimension = 0;
    this->xIndex = 0;
    this->yIndex = 0;
    this->zIndex = 0;
    this->m_varName = NULL;
    this->numParameterArrays = 0;
    this->coordDataSharedAcrossZones = 0;
    this->totalNumberOfElements = 0;
    this->totalNumberOfNodalPoints = 0;
    this->nodeOffset = 0;
    this->elementOffset = 0;
    this->vertex = NULL;
    this->parameterData = NULL;
    this->timeToInitVtk = NULL;
    this->numZonesAtTimestep = NULL;

    //Before anything can be run the manager must have been started

    boost::filesystem::path ext = boost::filesystem::path( this->inputFileNameAndPath ).extension();
    //std::cout << "\nextension = " << ext << std::endl;
    if( ext != ".dat" && ext != ".tec" && ext != ".plt" )
    {
        std::cerr << "\nWarning: Different extension than expected on input file '"
                  << this->inputFileNameAndPath << "'.  ";
        std::cerr << "Ascii tecplot files typically have extensions '.dat' or '.tec', ";
        std::cerr << "while binary tecplot files typically have extension '.plt'." << std::endl;
    }

    boost::filesystem::path full_path = boost::filesystem::path( this->inputFileNameAndPath );

    if ( boost::filesystem::exists( full_path ) )
    {
        std::cout << "\nReading file '" << this->inputFileNameAndPath << "'" << std::endl;
        this->ComputeNumberOfTimesteps();
        this->ComputeDimension();
        if( this->dimension == 0 )
        {
            std::cerr << "Error: input file did not contain coordinate data." << std::endl;
            this->numberOfTimesteps = 0; //set to zero so program will gracefully exit
            return;
        }
        this->SeeIfDataSharedAcrossZones();
    }
    else
    {
        std::cerr << "Error: input file does not exist or is not readable." << std::endl;
    }
}

tecplotReader::~tecplotReader()
{
#ifdef PRINT_HEADERS
    std::cerr << "deleting tecplotReader\n" << std::endl;
#endif // PRINT_HEADERS
    if( this->multiblock )
    {
        this->multiblock->Delete();
        this->multiblock = NULL;
    }

    if( this->ugrid )
    {
        this->ugrid->Delete();
        this->ugrid = NULL;
    }

    for( int i = 0; i < this->numVars; i++ )
    {
        TecUtilStringDealloc( &this->m_varName[ i ] );
    }
    delete [] m_varName;

    if( this->timeToInitVtk )
    {
        delete [] this->timeToInitVtk;
        this->timeToInitVtk = NULL;
    }

    if( this->numZonesAtTimestep )
    {
        delete [] this->numZonesAtTimestep;
        this->numZonesAtTimestep = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::SetMultiBlockOn()
{
    this->multiblockOutput = true;
}
////////////////////////////////////////////////////////////////////////////////
int tecplotReader::GetNumberOfTimesteps()
{
    return this->numberOfTimesteps;
}
////////////////////////////////////////////////////////////////////////////////
EntIndex_t tecplotReader::GetNumZonesInCurrentFile( const EntIndex_t startZone )
{
    EntIndex_t count = 1;
    for( EntIndex_t currentZone = startZone; currentZone < this->numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        if( this->timeToInitVtk[ currentZone ] == 0 )   // look ahead to next zone
        {
            count++;
        }
        else
        {
            return count;
        }
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
vtkDataObject * tecplotReader::GetOutput( const int timestep )
{
    // Verify that timestep is an appropriate zero-based integer...
    if( timestep < 0 || timestep > this->numberOfTimesteps - 1 )
    {
        std::cerr << "Error: invalid request for file " << timestep << std::endl;
        return NULL;
    }

    ///This will return a non zero number for transient files for everything
    ///with a time step greater than 0.
    int startZone = this->GetStartingZoneForTimestep( timestep );
    ///The number of zones for a given/requested timesetp. 
    int endZone = this->GetEndingZoneForTimestep( timestep );

    EntIndex_t numZonesInCurrentFile = this->GetNumZonesInCurrentFile( startZone );
    ///Now we know how many and which zones to process for a given/requested
    ///timesetp.
#ifdef PRINT_HEADERS
    std::cout << "startZone = " << startZone << ", numZonesInCurrentFile = " << numZonesInCurrentFile << std::endl;
#endif // PRINT_HEADERS

    int parNum = 0;

    if( multiblockOutput )
    {
#ifdef PRINT_HEADERS
        std::cout << "creating new multiblock with " << numZonesInCurrentFile << " zones" << std::endl;
#endif // PRINT_HEADERS
        if( this->multiblock )
        {
            this->multiblock->Delete();
            this->multiblock = 0;
        }
        this->multiblock = vtkMultiBlockDataSet::New();
    }
    
    // Begin at startZone and loop among the zones until we complete the file that was requested...
    for( EntIndex_t currentZone = startZone; currentZone < startZone+numZonesInCurrentFile; ++currentZone ) // zone numbers are 1-based
    {
        if( currentZone == startZone || multiblockOutput )
        {
            // Initialize the ugrid if working on first zone of a new grid...
            this->InitializeVtkData();
            parNum = 0;
        }

        this->ReadZoneName( currentZone );

        // Declare some variables to be passed among the next group of functions...
        // (Perhaps make these member variables and then argument lists will disappear)
        ZoneType_e zoneType;
        LgIndex_t numElementsInZone;
        int numNodesPerElement, numFaces, numNodalPointsInZone;
        this->ReadElementInfoInZone( currentZone, zoneType, numElementsInZone, numNodesPerElement,
                                     numFaces, numNodalPointsInZone );

        if( numFaces == 0 )
        {
            this->AddCellsToGrid( currentZone, zoneType, numElementsInZone, numNodesPerElement );
        }
        else
        {
            // face-based FE-data (polygons and polyhedrons)
            this->AddFaceCellsToGrid( currentZone, zoneType, numElementsInZone );
        }

        this->ReadNodalCoordinates( currentZone, numNodalPointsInZone );
        this->ReadNodeAndCellData( currentZone, numElementsInZone, numNodalPointsInZone, parNum );

        if( multiblockOutput )
        {
            this->AttachPointsAndDataToGrid();
#ifdef PRINT_HEADERS
            std::cout << "setting multiblock " << currentZone-startZone << std::endl;
#endif // PRINT_HEADERS
            this->multiblock->SetBlock( currentZone-startZone, this->ugrid );
        }

        // If this is last zone in the timestep, then clean up and return vtkDataObject
        if( currentZone == endZone )
        {
            if( multiblockOutput )
            {
/*
//Attaching a field to a multiblock is not working. Apparently a vtk bug. 
//See http://www.mail-archive.com/paraview@paraview.org/msg10434.html

                // Get the Solution Time associated with the specified zone.
                double currentSolutionTime = TecUtilZoneGetSolutionTime( currentZone );

                vtkFieldData * fd = vtkFieldData::New();

                vtkFloatArray * fa = vtkFloatArray::New();
                fa->SetName( "timestep" );
                fa->SetNumberOfComponents( 1 );
                fa->SetNumberOfTuples( 1 );
                fa->InsertTuple1( 0, currentSolutionTime );
                fd->AddArray( fa );
                fa->Delete();
                
                this->multiblock->SetFieldData( fd );
                fd->Delete();

#ifdef PRINT_HEADERS
                std::cout << "solutionTime = " << currentSolutionTime << std::endl;

                int numFieldArrays = this->multiblock->GetFieldData()->GetNumberOfArrays();
                std::cout << "numFieldArrays = " << numFieldArrays << std::endl;

                std::cout << "field data name = " << 
                this->multiblock->GetFieldData()->GetArray( 0 )->GetName()
                << std::endl;

                std::cout << "GetNumberOfComponents = " << 
                this->multiblock->GetFieldData()->GetArray( 0 )->GetNumberOfComponents()
                << std::endl;

                std::cout << "GetNumberOfTuples = " << 
                this->multiblock->GetFieldData()->GetArray( 0 )->GetNumberOfTuples()
                << std::endl;

                float value = this->multiblock->GetFieldData()->GetArray( 0 )->GetTuple1( 0 );
                std::cout << "field data value = " << value << std::endl;
#endif // PRINT_HEADERS

*/

#ifdef PRINT_HEADERS
                std::cout << "returning multiblock" << std::endl;
#endif // PRINT_HEADERS
                return this->multiblock;
            }
            else
            {
                // If the grid is complete, then attach data and return the ugrid...
                this->AttachPointsAndDataToGrid();
                return this->ugrid;
            }
        }
    }

    std::cerr << "Error: should not be here" << std::endl;
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
/*
Tecplot files can have one or more zones. Each zone contains a single element type (bricks, tetrahedrons, etc).
Each zone will have the same list of variable names (which will include 1d, 2d, or 3d coordinate data).
Data can be shared across zones, so constant data does not have to be repeated.

The tecplot files I have seen thus far include:

    INPUT                                                   OUTPUT
    ---------------------------------------------------     ---------------------------------------------------
1) The simple case
    1 zone
    1 set of nodal coordinates                              1 vtk unstructured grid with m variables
    1 nodal connectivity array
    m variables in addition to nodal coordinates

2) Zones are used to store variable values at different time steps in a single file
    n zones
    1 set of nodal coordinates, shared across all zones     n vtk unstructured grids, each with m variables
    1 nodal connectivity array, shared across all zones     -OR-
    m variables in addition to nodal coordinates,           1 vtk unstructured grid with >m variables
      some of which may be shared across all zones          (numVariables = n*numNotShared + numShared)

3) Zones represent same part which is deformed at different time steps
    n zones
    n sets of nodal coordinates                             n vtk unstructured grids, each with m variables
    1 nodal connectivity array
    m variables in addition to nodal coordinates,
      some of which may be shared across all zones

4) Zones represent sub-parts of a single part
    n zones
    n sets of nodal coordinates                             1 vtk unstructured grid with m variables
    n nodal connectivity arrays
    m variables in addition to nodal coordinates

5) Zones represent sub-parts of a single part, and zoneNames are repeated n times to indicate transient data
    k*n zones, each timestep comprising k zones
    n sets of nodal coordinates                             n vtk unstructured grids, each with m variables
    n nodal connectivity arrays
    m variables in addition to nodal coordinates

Tecplot tells us numZones & numVars. We can look at the dimension of the coordinate data, and solve,
m variables = numVars - dimension.

coordDataSharedAcrossZones == 1 means that there is 1 set of nodal coordinates, shared across all zones

if connectivityShareCount == numZones, then there is just 1 nodal connectivity array
*/

void tecplotReader::ReadVariable( const EntIndex_t currentZone, int varNumber, const char* varName, vtkFloatArray* scalarData )
{
    // Read a single variable from the current zone...
    if( varNumber )
    {
        FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef( currentZone, varNumber ); //1-based
        if( FieldData )
        {
            LgIndex_t numValues = TecUtilDataValueGetCountByRef( FieldData );

            /*if( scalarData == NULL )
            {
                scalarData = vtkFloatArray::New();
                scalarData->SetName( varName );
                scalarData->SetNumberOfComponents( 1 );
            }*/

#ifdef PRINT_HEADERS
            std::cout << "reading parameter " << varNumber << " '"
                << varName << "' from zone " << currentZone
                << ", numValues = " << numValues << std::endl;
#endif // PRINT_HEADERS
            for( LgIndex_t i = 0; i < numValues; ++i )
            {
                //GetByRef function is 1-based
                double tempData = TecUtilDataValueGetByRef( FieldData, i+1 );
                scalarData->InsertNextTuple( &tempData );
            }
        }
        else
        {
            std::cerr << "Error: Unable to read " << varName << " variable data" << std::endl;
        }
    }
    else
    {
        std::cerr << "Error: variable number " << varNumber << " does not exist or can not be read" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
vtkFloatArray * tecplotReader::ZeroArray( std::string varName, int numTuples )
{
#ifdef PRINT_HEADERS
    std::cout << "setting parameter '" << varName << "' to zero" << std::endl;
#endif // PRINT_HEADERS
    vtkFloatArray* zero = vtkFloatArray::New();
    zero->SetName( varName.c_str() );
    zero->SetNumberOfTuples( numTuples );
    zero->SetNumberOfComponents( 1 );
    for( int i = 0; i < numTuples; i++ )
    {
        zero->SetValue( i, 0.0 );
    }
    return zero;
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ProcessAnyVectorData( vtkFloatArray ** vectorData )
{
    // Now see if any variable names appear to be representing vector quantities...

    std::string vecName;
    std::string midName, previousMidName, nextMidName;

    int repeatCount = 0;

    // skipping first few parameters (point coordiantes), examine variable names
    for( int i = this->dimension; i < this->numVars; i++ )
    {
        // extract the middle part of the variable name -- neglect the first and last character
        std::string varName( this->m_varName[ i ] );
        midName = varName.substr( 1, varName.length()-2 );

        if( midName.length() > 0 && midName.compare( previousMidName ) == 0 )
        {
            repeatCount++;
        }
        else 
        {
            repeatCount = 1;
        }

        if( repeatCount == this->dimension ) // might have found a vector, need to do a few more checks
        {
            bool foundVector = false;

            if( i == this->numVars-1 ) // if at end of parameter list
            {
                foundVector = true;
            }
            else 
            {
                //look at next parameter name, again neglecting the first and last character
                //we do this because most vector are comprised of scalars that
                //either begin/end with u,v,w or x,y,z or 1,2,3. By stripping 
                //these characters and comparing the middle portion of a scalar
                //name we can actually determine if 3 scalars make up
                //a vector.
                std::string nextVarName( this->m_varName[ i+1 ] );
                nextMidName = nextVarName.substr( 1, varName.length()-2 );
                // if name is different, then assume we found a vector
                if( midName.compare( nextMidName ) != 0 )
                {
                    foundVector = true;
                }
            }
            
            if ( foundVector )  // reconstruct vector name
            {
                std::string previousVarName( this->m_varName[ i-1 ] );
                //if first char was same then put it back
                if( varName.compare( 0, 1, previousVarName, 0, 1 ) == 0 )
                {
                    vecName = varName.substr( 0, varName.length()-1 );
                }
                //if last char was same then put it back
                else if( varName.compare( varName.length(), 1, previousVarName, varName.length(), 1 ) == 0 )
                {
                    vecName = varName.substr( 1, varName.length()-1 );
                }
                else
                {
#ifdef PRINT_HEADERS
                    std::cerr << "Unexpected variable name: Should not get here" << std::endl;
#endif // PRINT_HEADERS
                    vecName = midName;
                }

                // remove leading and trailing whitespace and hyphens
                boost::trim_left_if( vecName, boost::algorithm::is_any_of("- ") );
                boost::trim_right_if( vecName, boost::algorithm::is_any_of("- ") );
                //So that the vector name is reasonable
                int vectorIndex[ 3 ] = { 0, 0, 0 };
                if( this->dimension == 2 )
                {
                    vectorIndex[ 0 ] = i;
                    vectorIndex[ 1 ] = i + 1;
                }
                else if( this->dimension == 3 )
                {
                    vectorIndex[ 0 ] = i - 1;
                    vectorIndex[ 1 ] = i;
                    vectorIndex[ 2 ] = i + 1;
                }
#ifdef PRINT_HEADERS
                std::cout << "Found vector '" << vecName << "'"
                          << ", vectorIndex = " << vectorIndex[ 0 ] << " "
                          << vectorIndex[ 1 ] << " " << vectorIndex[ 2 ] << std::endl;
#endif // PRINT_HEADERS

                vtkIdType numTuples = vectorData[ vectorIndex[ 0 ]-1-this->dimension ]->GetNumberOfTuples();
                bool isPointData = true;
                vtkIdType pointCount = 0;
                if( numTuples == this->totalNumberOfNodalPoints )
                {
                    isPointData = true;
                    pointCount = totalNumberOfNodalPoints;
                }
                else if( numTuples == this->totalNumberOfElements )
                {
                    isPointData = false;
                    pointCount = totalNumberOfElements;
                }
                
                vtkFloatArray* vector = vtkFloatArray::New();
                vector->SetName( vecName.c_str() );
                vector->SetNumberOfTuples( pointCount );
                vector->SetNumberOfComponents( 3 );

                for( int j = 0; j < pointCount; j++ )
                {
                    for( int k = 0; k < 3; k++ )
                    {
                        if( vectorIndex[ k ] == 0 )
                        {
                            vector->InsertComponent( j, k, 0.0 );
                        }
                        else
                        {
                            //cout << "copying data from " << vectorData[ vectorIndex[ j ]-1-this->dimension ]->GetName() 
                            //    << ", value = " << parameterData[ vectorIndex[ j ]-1-this->dimension ]->GetValue( i ) << std::endl;
                            vector->InsertComponent( j, k, vectorData[ vectorIndex[ k ]-1-this->dimension ]->GetValue( j ) );
                        }
                    }
                }

                if( isPointData )
                {
                    this->ugrid->GetPointData()->AddArray( vector );
                }
                else
                {
                    this->ugrid->GetCellData()->AddArray( vector );
                }
                vector->Delete();
            }
        }

        previousMidName = midName;
    }
    return;
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ComputeNumberOfTimesteps()
{
    StringList fileName( this->inputFileNameAndPath.c_str(), NULL );

    Boolean_t IsOk = TecUtilReadDataSet(
        ReadDataOption_NewData,    // NewData = Remove data set fron current frame before loading new data set
        TRUE,                      // ResetStyle is TRUE if you want to reset the style of the current frame
        fileName.getRef(),         // string list containing the file name(s) to load
        "TECPLOT",                 // DataSetReader
        PlotType_Automatic,        // InitialPlotType
        TRUE, TRUE, TRUE, TRUE,    // IncludeText, IncludeGeom, IncludeCustomLabels, IncludeData
        FALSE,                     // CollapseZonesAndVars = TRUE to renumber zones and variables if any are disabled
        NULL,                      // ZonesToRead, Use NULL to load all zones
        VarLoadMode_ByName,        // VarLoadMode is either ByName or ByPosition
        NULL,                      // VarPositionList is used only if VarLoadMode is ByPosition. Use NULL to load all variables.
        NULL,                      // VarNameList, Use NULL to load only variable names common to all data files.
        1, 1, 1 );                 // Set to 1 to load every data point in the I-direction, J-direction, & K-direction.

    if( ! IsOk )
    {
        std::cerr << "Error: The dataset could not be read." << std::endl;
        this->numberOfTimesteps = 0;
        return;
    }

    char *dataset_title = NULL;
    TecUtilDataSetGetInfo( &dataset_title, &this->numZones, &this->numVars );
#ifdef PRINT_HEADERS
    std::cout << "The dataset_title is \"" << dataset_title << "\"" << std::endl;
    std::cout << "Number of zones in entire dataset is " << this->numZones << " and number of variables is " << this->numVars << std::endl;
#endif // PRINT_HEADERS
    TecUtilStringDealloc( &dataset_title );

    // Is data shared across zones? (typically coordinate data or cell data)
    // Appear to get same result regardless of zone, so just look at first zone
    this->connectivityShareCount = TecUtilDataConnectGetShareCount( 1 );
#ifdef PRINT_HEADERS
    std::cout << "Connectivity share count of zone 1 is " << this->connectivityShareCount << std::endl;
#endif // PRINT_HEADERS

    // Allocate memory for array that correlates zones to output files...
    // The size of this array is one larger than numZones: last index is used to 'terminate' the list...
    this->timeToInitVtk = new int [ this->numZones + 1 ];

    // Use tecplot's StrandId and Solution Time to determine corelation between zones and output files...
    this->CountNumberOfTimestepsUsingSolnTime();

    // Look at a special case where StrandId is not used...
    // If n zones and shared connectivity, then the file type is transient.
    // Each zone will be a unique file.
    if( this->numZones > 1 && this->connectivityShareCount == this->numZones )
    {
#ifdef PRINT_HEADERS
        std::cout << "Special case where StrandId is not used. n zones with shared connectivity, so file type is transient." << std::endl;
#endif // PRINT_HEADERS
        this->numberOfTimesteps = this->numZones;
        for( int j = 0; j < this->numZones + 1; j++ )
        {
            this->timeToInitVtk[ j ] = 1;
        }
    }

#ifdef PRINT_HEADERS
    std::cout << "NumberOfTimesteps = " << this->numberOfTimesteps << std::endl;
    for( int j = 0; j < this->numZones + 1; j++ )
    {
        std::cout << "timeToInitVtk[ " << j << " ] = " << this->timeToInitVtk[ j ] << std::endl;
    }
#endif // PRINT_HEADERS

    this->numZonesAtTimestep = new int [ this->numberOfTimesteps ];

    for( int timestep = 0; timestep < this->numberOfTimesteps; timestep++ )
    {
        int startZone = this->GetStartingZoneForTimestep( timestep );
        EntIndex_t numZonesInCurrentFile = this->GetNumZonesInCurrentFile( startZone );
        this->numZonesAtTimestep[ timestep ] = numZonesInCurrentFile;
    }

#ifdef PRINT_HEADERS
    std::cout << "numZonesAtTimestep = { ";
    for( int timestep = 0; timestep < this->numberOfTimesteps; timestep++ )
    {
        std::cout << this->numZonesAtTimestep[ timestep ] << " ";
    }
    std::cout << "}" << std::endl;
#endif // PRINT_HEADERS
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ComputeDimension()
{
    int numNonCoordinateParameters = 0;

    this->m_varName = new VarName_t[ this->numVars ];

    //All TecPlot files have their coordinates in the first 3 vars of the 
    //variable arrays. Since we only support 2D - 3D data we can always assume
    //that X and Y are in indices 1 and 2. Therefore we only need to test the
    //3rd variable name for Z.
    bool hasZCoord = false;
    if( numVars >= 3 )
    {
        hasZCoord = TestForZVariable();
    }

    for( int i = 0; i < this->numVars; ++i )
    {
        // Read ith variable name...
        m_varName[ i ] = 0;
        TecUtilVarGetName( i+1, &this->m_varName[ i ] ); // variable numbers are 1-based
#ifdef PRINT_HEADERS
        std::cout << "The name of Variable " << i+1 << " is \"" << this->m_varName[ i ] << "\"" << std::endl;
#endif // PRINT_HEADERS

        // If this variable name corresponds to coordinate data, then record the 1-based index...
        if( i == 0 ) 
        {
            this->xIndex = i+1;
            this->dimension++;
        }
        else if( i == 1 ) 
        {
            this->yIndex = i+1;
            this->dimension++;
        }
        else if( (i == 2) && hasZCoord )
        {
            this->zIndex = i+1;
            this->dimension++;
        }
        else
        {
            // count number of non-coordinate nodal data parameters
            numNonCoordinateParameters++;
        }
    }

#ifdef PRINT_HEADERS
    std::cout << "dimension is " << this->dimension << ", xIndex is "
        << this->xIndex << ", yIndex is " << this->yIndex
        << ", zIndex is " << this->zIndex << std::endl;
    std::cout << "numNonCoordinateParameters is "
        << numNonCoordinateParameters << std::endl;
#endif // PRINT_HEADERS

    // Prepare to count the number of non-coordinate parameter arrays
    this->numParameterArrays = numNonCoordinateParameters;
#ifdef PRINT_HEADERS
    std::cout << "numParameterArrays = "
        << this->numParameterArrays << std::endl;
#endif // PRINT_HEADERS
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::SeeIfDataSharedAcrossZones()
{
    this->coordDataSharedAcrossZones = 0;

    // Is data shared across zones? (typically coordinate data or cell data)
    //for( EntIndex_t currentZone = 1; currentZone < this->numZones+1; currentZone++ ) // zone numbers are 1-based
    // Appear to get same result regardless of zone, so just look at first zone
    EntIndex_t currentZone = 1; // zone numbers are 1-based
    {
        int tecplotVar = 0;
        for( int i = 0; i < this->numVars; i++ )
        {
            EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
#ifdef PRINT_HEADERS
            std::cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << std::endl;
#endif // PRINT_HEADERS

            tecplotVar = 1 + i;
            // If variable name corresponds to one of the coordinate labels...
            /*if( strcmp( this->m_varName[ i ], "X" ) == 0 ||
                strcmp( this->m_varName[ i ], "Y" ) == 0 ||
                strcmp( this->m_varName[ i ], "Z" ) == 0 )*/
            if( (tecplotVar == xIndex) || (tecplotVar == yIndex) || (tecplotVar == zIndex) )
            {
                if( dataShareCount == this->numZones )
                {
                    // coordinate data is shared across zones
                    this->coordDataSharedAcrossZones = 1;
                    //this->numParameterArrays = numNonCoordinateParameters * this->numZones;
                }
                else
                {
                    // coordinate data is specified for each zone
                    //this->numParameterArrays = numNonCoordinateParameters;
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::InitializeVtkData()
{
    // Initialize the ugrid
    if( this->ugrid )
    {
        this->ugrid->Delete();
        this->ugrid = NULL;
    }
    this->ugrid = vtkUnstructuredGrid::New();
    this->vertex = vtkPoints::New();

    // set up arrays to store scalar nodal & element data over entire mesh...
    this->parameterData = new vtkFloatArray* [ this->numParameterArrays ];
    for( int i = 0; i < this->numParameterArrays; ++i )
    {
        this->parameterData[ i ] = vtkFloatArray::New();
        parameterData[ i ]->SetNumberOfComponents( 1 );
        //parameterData[ i ]->DebugOn();
    }

    int paramCounter = 0;
    int tecplotVar = 0;
    for( int i = 0; i < numVars; ++i )
    {
        tecplotVar = i + 1;
        if( (tecplotVar != xIndex) && (tecplotVar != yIndex) && (tecplotVar != zIndex) )
        {
            parameterData[ paramCounter ]->SetName( m_varName[ i ] );
            paramCounter++;
        }
    }

    this->nodeOffset = 0;
    this->elementOffset = 0;
    this->totalNumberOfNodalPoints = 0;
    this->totalNumberOfElements = 0;
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::CountNumberOfTimestepsUsingSolnTime()
{
    // Get the StrandID associated with the first zone.
    Strand_t previousStrandID = TecUtilZoneGetStrandID( 1 );

    // Get the Solution Time associated with the first zone.
    double previousSolutionTime = TecUtilZoneGetSolutionTime( 1 );
#ifdef PRINT_HEADERS
    std::cout << "for zone 1, strandID = " << previousStrandID 
        << ", solutionTime = " << previousSolutionTime << std::endl;
#endif // PRINT_HEADERS

    this->numberOfTimesteps = 1;
    this->timeToInitVtk[ 0 ] = 1;

    // Look at the rest of the zones...
    for( EntIndex_t currentZone = 2; currentZone < this->numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        this->timeToInitVtk[ currentZone-1 ] = 0;

        // Get the StrandID associated with the specified zone.
        Strand_t currentStrandID = TecUtilZoneGetStrandID( currentZone );

        // Get the Solution Time associated with the specified zone.
        double currentSolutionTime = TecUtilZoneGetSolutionTime( currentZone );
#ifdef PRINT_HEADERS
        std::cout << "for zone " << currentZone << ", strandID = " << currentStrandID 
            << ", solutionTime = " << currentSolutionTime << std::endl;
#endif // PRINT_HEADERS

        // StrandID of 0 indicates that the zone is not part of a strand (a static zone).
        // Transient zones will have a StrandID of 1 or greater. See sdkum_qt.pdf Section 15 - 11
        // A change from static to transient (or vice versa) will be detected by ...
        bool strandIdShowsChangeFromStaticToTransientOrViceVersa =
            ( previousStrandID * currentStrandID ) == 0 &&      // this tests that one of the two are zero
            ( previousStrandID + currentStrandID ) > 0;         // this tests that both are not zero

        // We want to know the number of files that we will output.
        // For transient data, we will have one file for each timestep. Each timestep may include multiple zones.
        // In tecplot, files are not just static or transient -- There may be static zones interspersed with transient zones.
        // When go from static to transient (or vice versa) or when solution time changes between zones, increment number...
        if( strandIdShowsChangeFromStaticToTransientOrViceVersa || previousSolutionTime != currentSolutionTime )
        {
            this->numberOfTimesteps++;
            this->timeToInitVtk[ currentZone-1 ] = 1;
        }

        // Update values from last loop...
        previousStrandID = currentStrandID;
        previousSolutionTime = currentSolutionTime;
    }
    // The last index is set to one to 'terminate' the list...
    this->timeToInitVtk[ this->numZones ] = 1;
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ReadElementInfoInZone( const EntIndex_t currentZone, ZoneType_e& zoneType, LgIndex_t& numElementsInZone,
                          int& numNodesPerElement, int& numFaces, int& numNodalPointsInZone )
{
#ifdef PRINT_HEADERS
    std::cout << "zoneType is ";
#endif // PRINT_HEADERS
    // read zoneType, numNodalPointsInZone, numElementsInZone, nodal connectivity
    // (read it again, even if it was shared and you read it already)
    zoneType = TecUtilZoneGetType( currentZone );
    numElementsInZone = 0;

    switch( zoneType )  // compare to those defined in GLOBAL.h
    {
        case ZoneType_Ordered:
#ifdef PRINT_HEADERS
            std::cout << "Ordered" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FETriangle:
#ifdef PRINT_HEADERS
            std::cout << "FETriangle" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FEQuad:
#ifdef PRINT_HEADERS
            std::cout << "FEQuad" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FETetra:
#ifdef PRINT_HEADERS
            std::cout << "FETetra" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FEBrick:
#ifdef PRINT_HEADERS
            std::cout << "FEBrick" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FELineSeg:
#ifdef PRINT_HEADERS
            std::cout << "FELineSeg" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FEPolygon:
#ifdef PRINT_HEADERS
            std::cout << "FEPolygon" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_FEPolyhedron:
#ifdef PRINT_HEADERS
            std::cout << "FEPolyhedron" << std::endl;
#endif // PRINT_HEADERS
            break;
        case END_ZoneType_e:
#ifdef PRINT_HEADERS
            std::cout << "END_ZoneType_e" << std::endl;
#endif // PRINT_HEADERS
            break;
        case ZoneType_Invalid:
#ifdef PRINT_HEADERS
            std::cout << "Invalid" << std::endl;
#endif // PRINT_HEADERS
            break;
        default:
            std::cerr << "ZoneType not recognized. Not supposed to get here." << std::endl;
            break;
    }

    // Obtain information about the current zone.
    // If the frame mode is XY the handles must be passed in as NULL.
    // Otherwise, passing NULL indicates the value is not desired.
    LgIndex_t IMax, JMax, KMax;
    TecUtilZoneGetInfo(
        currentZone,    // Number of the zone to query
        &IMax,          // Receives the I-dimension for ordered data. Number of data points for FE-data.
        &JMax,          // Receives the J-dimension for ordered data. Number of elements for FE-data.
        &KMax,          // Receives the K-dimension for ordered data.
                        // Number of nodes per cell for cell-based FE-data (triangle, brick, tetrahedral, quadtrilateral).
                        // Number of faces for face-based FE-data (polygons and polyhedrons).
        NULL,           // Receives the handle to a writeable field data for X.
        NULL,           // Receives the handle to a writeable field data for Y.
        NULL,           // Receives the handle to a writeable field data for Z.
        NULL,           // Receives the handle for a writeable connectivity list.
        NULL,           // Receives the Handle to a writeable field data for U.
        NULL,           // Receives the handle to a writable field data for V.
        NULL,           // Receives the handle to a writable field data for W.
        NULL,           // Receives the handle to a writable field data for the blanking variable.
        NULL,           // Receives the handle to a writable field data for the contouring variable.
        NULL );         // Receives the handle to a writable field data for the scatter sizing variable.

    numNodesPerElement = 0;
    numFaces = 0;
    numNodalPointsInZone = 0;

    if( zoneType == ZoneType_Ordered )
    {
#ifdef PRINT_HEADERS
        std::cout << "   The I-dimension for ordered data is " << IMax << std::endl;
        std::cout << "   The J-dimension for ordered data is " << JMax << std::endl;
        std::cout << "   The K-dimension for ordered data is " << KMax << std::endl;
#endif // PRINT_HEADERS
        if( IMax > 0 && JMax > 0 && KMax > 0 )
        {
            numNodesPerElement = 8;
        }
        else if( ( IMax > 0 && JMax > 0 ) ||
                 ( IMax > 0 && KMax > 0 ) ||
                 ( JMax > 0 && KMax > 0 )  )
        {
            numNodesPerElement = 4;
        }
        else if( IMax > 0 || JMax > 0 || KMax > 0 )
        {
            numNodesPerElement = 2;
        }
        else
        {
#ifdef PRINT_HEADERS
            std::cout << "IMax = JMax = KMax = 0. Not supposed to get here." << std::endl;
#endif // PRINT_HEADERS
            numNodesPerElement = 0;
        }
    }
    else if( zoneType > ZoneType_Ordered && zoneType < END_ZoneType_e )
    {
        numNodalPointsInZone = IMax;  // tecplot called this 'number of data points' but that is misleading
#ifdef PRINT_HEADERS
        std::cout << "   The number of nodal points in this zone is " << numNodalPointsInZone << std::endl;
#endif // PRINT_HEADERS

        numElementsInZone = JMax;
#ifdef PRINT_HEADERS
        std::cout << "   The number of elements in this zone is " << numElementsInZone << std::endl;
#endif // PRINT_HEADERS

        if( zoneType == ZoneType_FEPolygon || zoneType == ZoneType_FEPolyhedron )
        {
            // face-based FE-data
            numFaces = KMax;
#ifdef PRINT_HEADERS
            std::cout << "   The number of faces is " << numFaces << std::endl;
#endif // PRINT_HEADERS
        }
        else
        {
            // cell-based FE-data
            numNodesPerElement = KMax;
#ifdef PRINT_HEADERS
            std::cout << "   The number of nodes per cell is " << numNodesPerElement << std::endl;
#endif // PRINT_HEADERS
        }
    }
    else
    {
        std::cerr << "ZoneType not known. Not supposed to get here." << std::endl;
    }

    if( currentZone > 1 && this->numZones > 1 && !this->coordDataSharedAcrossZones && this->connectivityShareCount == 1 )
    {
        this->totalNumberOfNodalPoints += numNodalPointsInZone;
        this->totalNumberOfElements += numElementsInZone;
    }
    else
    {
        this->totalNumberOfNodalPoints = numNodalPointsInZone;
        this->totalNumberOfElements = numElementsInZone;
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ReadZoneName( const EntIndex_t currentZone )
{
    VarName_t* zoneName = new VarName_t [ this->numZones ];
    zoneName[ currentZone - 1 ] = 0;
    if( TecUtilZoneGetName( currentZone, &zoneName[ currentZone - 1 ] ) )
    {
#ifdef PRINT_HEADERS
        std::cout << "For Zone " << currentZone << ", zoneName is \"" 
            << zoneName[ currentZone - 1 ] << "\""  << std::endl;
#endif // PRINT_HEADERS
        TecUtilStringDealloc( &zoneName[ currentZone - 1 ] );
    }
    else
    {
        std::cerr << "Error: Unable to get name of zone " << currentZone << std::endl;
    }
    delete [] zoneName;
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::AddCellsToGrid( const EntIndex_t currentZone, 
    const ZoneType_e zoneType, const LgIndex_t numElementsInZone, const int numNodesPerElement )
{
/*
    EntIndex_t this->connectivityShareCount = TecUtilDataConnectGetShareCount( currentZone );
    std::cout << "Connectivity share count of current zone is " << this->connectivityShareCount << std::endl;
*/

    // define flag to control printing of unique error message to a single time...
    int undefinedZoneType[ 8 ] = {1,1,1,1,1,1,1,1};

    // For each tecplot element in current zone, construct corresponding VTK element
    // read nodal connectivity (read it again, even if it was shared and you read it already)
    NodeMap_pa nm = TecUtilDataNodeGetReadableRef( currentZone );
    if( nm && (numNodesPerElement > 0) )
    {
        vtkIdList* tempIdList = vtkIdList::New();
        tempIdList->SetNumberOfIds( numNodesPerElement );
        vtkIdType nodeValue;

        for( LgIndex_t elemNum = 1; elemNum < numElementsInZone+1; elemNum++ ) // element numbers are 1-based
        {
            // Node information (connectivity)
            // NOTE - You could use the "RawPtr" functions if speed is a critical issue
            //std::cout << "For element " << elemNum << ", nodes =";
            for( int i = 0; i < numNodesPerElement; i++ )
            {
                // node numbers in tecplot are 1-based, 0-based in VTK
                nodeValue = TecUtilDataNodeGetByRef( nm, elemNum, i+1 ) - 1 + this->nodeOffset;
                //std::cout << " " << nodeValue;
                tempIdList->SetId( i, nodeValue );
            }
            //std::cout << std::endl;

            if( zoneType == ZoneType_FETriangle )
            {
                this->ugrid->InsertNextCell( VTK_TRIANGLE, tempIdList );
            }
            else if( zoneType == ZoneType_FEQuad )
            {
                this->ugrid->InsertNextCell( VTK_QUAD, tempIdList );
            }
            else if( zoneType == ZoneType_FETetra )
            {
                // Some files such as ansys11_washer end up with "invisible" cells.
                // It appears to be because some "tetrahedrons" are actually points, lines, or triangles.
                // Check for repeating indexes in the nodeList...
                int noRepeatingIndexes = 1;
                for( int i = 0; i < numNodesPerElement && noRepeatingIndexes == 1; i++ )
                {
                    int firstIndex = tempIdList->GetId( i );
                    for( int j = i+1; j < numNodesPerElement && noRepeatingIndexes == 1; j++ )
                    {
                        int secondIndex = tempIdList->GetId( j );
                        if( firstIndex == secondIndex )
                        {
#ifdef PRINT_HEADERS
                            std::cout << "bad element found!!!!!   ";
                            for( int k = 0; k < numNodesPerElement; k++ )
                            {
                                std::cout << " " << tempIdList->GetId( k );
                            }
                            std::cout << std::endl;
#endif // PRINT_HEADERS
                            noRepeatingIndexes = 0;
                        }
                    }
                }

                if( noRepeatingIndexes )
                {
                    this->ugrid->InsertNextCell( VTK_TETRA, tempIdList );
                }
            }
            else if( zoneType == ZoneType_FEBrick )
            {
                this->ugrid->InsertNextCell( VTK_HEXAHEDRON, tempIdList );
            }
            else
            {
                if( undefinedZoneType[ zoneType ] )
                {
                    std::cerr << "Error: Can not yet handle element type " << zoneType
                         << ", numNodesPerElement = " << numNodesPerElement << std::endl;
                    undefinedZoneType[ zoneType ] = 0;  // set flag so as to not print more than one error msg
                }

                this->ugrid->InsertNextCell( VTK_EMPTY_CELL, 0, NULL );
            }
        }
        tempIdList->Delete();
    }
    else
    {
        std::cerr << "Error: Unable to get node map" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::AddFaceCellsToGrid( const EntIndex_t currentZone, 
    const ZoneType_e zoneType, const LgIndex_t numElementsInZone )
{
    ElemToFaceMap_pa ElemToFaceMap = TecUtilDataElemGetReadableRef( currentZone );
    if( ! ElemToFaceMap ) 
    {
        std::cerr << "Warning: ElemToFaceMap came back null for zone " << currentZone << std::endl;
        return;
    }

    FaceMap_pa FaceMap = TecUtilDataFaceMapGetReadableRef( currentZone );
    if( ! FaceMap ) 
    {
        std::cerr << "Warning: FaceMap came back null for zone " << currentZone << std::endl;
        return;
    }

    if( zoneType == ZoneType_FEPolygon )
    {
        for( LgIndex_t elemNum = 1; elemNum < numElementsInZone+1; elemNum++ ) // element numbers are 1-based
        {
            int numberOfMatches = 0;

            LgIndex_t numFacesPerElement = TecUtilDataElemGetNumFaces( ElemToFaceMap, elemNum );
#ifdef PRINT_HEADERS
            if( elemNum < 10 )
            { 
                std::cout << "For elem " << elemNum 
                    << ", numFacesPerElement = " << numFacesPerElement << std::endl; 
            }
#endif // PRINT_HEADERS

            vtkIdList* tempIdList = vtkIdList::New();

            for( LgIndex_t faceOffset = 1; faceOffset < numFacesPerElement+1; faceOffset++ ) // numbers are 1-based
            {
                LgIndex_t faceNumber = TecUtilDataElemGetFace( ElemToFaceMap,  elemNum, faceOffset );

                // how many nodes comprise the specified face? (will return 2 for polygonal zones)
                LgIndex_t numNodesOnFace = TecUtilDataFaceMapGetNFaceNodes( FaceMap, faceNumber );
#ifdef PRINT_HEADERS
                if( elemNum < 10 )
                {
                    std::cout << "   For face " << faceNumber 
                        << ", numNodesOnFace = " << numNodesOnFace << ". node list = " ; 
                }
#endif // PRINT_HEADERS

                for( LgIndex_t node = 1; node < numNodesOnFace+1; node++ ) // numbers are 1-based
                {
                    // node numbers in tecplot are 1-based, 0-based in VTK
                    vtkIdType nodeValue = TecUtilDataFaceMapGetFaceNode( FaceMap, faceNumber, node ) - 1 + this->nodeOffset;
#ifdef PRINT_HEADERS
                    if( elemNum < 10 ) { std::cout << "  " << nodeValue; }
#endif // PRINT_HEADERS
                    tempIdList->InsertUniqueId( nodeValue );

                    // see if the second face contains the same node that ended the face one description:
                    if( faceOffset == 2 && nodeValue == tempIdList->GetId( 1 ) )
                    {
                        numberOfMatches++;
                    }
                }
#ifdef PRINT_HEADERS
                if( elemNum < 10 ) { std::cout << std::endl; }
#endif // PRINT_HEADERS
            }

            // If nodal order causes twisted element, then fix now:
            if( numberOfMatches == 0 )
            {
                vtkIdType ID[ 2 ];
                ID[ 0 ] = tempIdList->GetId( 0 );
                ID[ 1 ] = tempIdList->GetId( 1 );
                tempIdList->SetId( 0, ID[ 1 ] );
                tempIdList->SetId( 1, ID[ 0 ] );
            }
            else if( numberOfMatches > 1 )
            {
                std::cerr << "Error: numberOfMatches = " << numberOfMatches << std::endl;
            }

            this->ugrid->InsertNextCell( VTK_POLYGON, tempIdList );
            tempIdList->Delete();
        }
    }
    else if( zoneType == ZoneType_FEPolyhedron )
    {
#if ((VTK_MAJOR_VERSION == 5)&&(VTK_MINOR_VERSION > 6))
        for( LgIndex_t elemNum = 1; elemNum < numElementsInZone+1; elemNum++ ) // element numbers are 1-based
        {
            LgIndex_t numFacesPerElement = TecUtilDataElemGetNumFaces( ElemToFaceMap, elemNum );
#ifdef PRINT_HEADERS
            if( elemNum < 10 )
            {
                std::cout << "For elem " << elemNum 
                    << ", numFacesPerElement = " << numFacesPerElement << std::endl; 
            }
#endif // PRINT_HEADERS

            // Begin setting up the tempIdList. 
            // For polyhedron cell, a special format is required: (numCellFaces, numFace0Pts, id1, id2, id3, numFace1Pts,id1, id2, id3, ...) 
            vtkIdList* tempIdList = vtkIdList::New();
            tempIdList->InsertNextId( numFacesPerElement );

            for( LgIndex_t faceOffset = 1; faceOffset < numFacesPerElement+1; faceOffset++ ) // numbers are 1-based
            {
                LgIndex_t faceNumber = TecUtilDataElemGetFace( ElemToFaceMap,  elemNum, faceOffset );

                // how many nodes comprise the specified face?
                LgIndex_t numNodesOnFace = TecUtilDataFaceMapGetNFaceNodes( FaceMap, faceNumber );
#ifdef PRINT_HEADERS
                if( elemNum < 10 )
                { 
                    std::cout << "   For face " << faceNumber 
                    << ", numNodesOnFace = " << numNodesOnFace << ". node list = " ; 
                }
#endif // PRINT_HEADERS
                tempIdList->InsertNextId( numNodesOnFace );

                for( LgIndex_t node = 1; node < numNodesOnFace+1; node++ ) // numbers are 1-based
                {
                    // node numbers in tecplot are 1-based, 0-based in VTK
                    vtkIdType nodeValue = TecUtilDataFaceMapGetFaceNode( FaceMap, faceNumber, node ) - 1 + this->nodeOffset;
#ifdef PRINT_HEADERS
                    if( elemNum < 10 ) { std::cout << "  " << nodeValue; }
#endif // PRINT_HEADERS
                    tempIdList->InsertNextId( nodeValue );
                }
#ifdef PRINT_HEADERS
                if( elemNum < 10 ) { std::cout << std::endl; }
#endif // PRINT_HEADERS
            }

            this->ugrid->InsertNextCell( VTK_POLYHEDRON, tempIdList );
            tempIdList->Delete();
        }
#else // VTK_VERSION
        //std::cout << "Warning: VTK version " << VTK_VERSION 
        //    << " can not handle polyhedron cells in zone " << currentZone << std::endl;
        // Begin setting up the tempIdList. 
        vtkIdList* tempIdList = vtkIdList::New();
        for( LgIndex_t elemNum = 1; elemNum < numElementsInZone+1; ++elemNum ) // element numbers are 1-based
        {
            LgIndex_t numFacesPerElement = TecUtilDataElemGetNumFaces( ElemToFaceMap, elemNum );
#ifdef PRINT_HEADERS
            if( elemNum < 10 )
            { 
                std::cout << "For elem " << elemNum 
                    << ", numFacesPerElement = " << numFacesPerElement << std::endl; 
            }
#endif // PRINT_HEADERS
            for( LgIndex_t faceOffset = 1; faceOffset < numFacesPerElement+1; ++faceOffset ) // numbers are 1-based
            {
                LgIndex_t faceNumber = TecUtilDataElemGetFace( ElemToFaceMap,  elemNum, faceOffset );
                
                // how many nodes comprise the specified face?
                LgIndex_t numNodesOnFace = TecUtilDataFaceMapGetNFaceNodes( FaceMap, faceNumber );
#ifdef PRINT_HEADERS
                if( elemNum < 10 )
                {
                    std::cout << "   For face " << faceNumber 
                    << ", numNodesOnFace = " << numNodesOnFace << ". node list = " ; 
                }
#endif // PRINT_HEADERS
                
                for( LgIndex_t node = 1; node < numNodesOnFace+1; ++node ) // numbers are 1-based
                {
                    // node numbers in tecplot are 1-based, 0-based in VTK
                    vtkIdType nodeValue = 
                        TecUtilDataFaceMapGetFaceNode( FaceMap, faceNumber, node ) - 1 + this->nodeOffset;
#ifdef PRINT_HEADERS
                    if( elemNum < 10 ) { std::cout << "  " << nodeValue; }
#endif // PRINT_HEADERS
                    tempIdList->InsertUniqueId( nodeValue );
                }
#ifdef PRINT_HEADERS
                if( elemNum < 10 ) { std::cout << std::endl; }
#endif // PRINT_HEADERS
            }
            
            this->ugrid->InsertNextCell( VTK_CONVEX_POINT_SET, tempIdList );
            tempIdList->Reset();
        }
        tempIdList->Delete();
#endif // VTK_VERSION
    }
    else
    {
        std::cerr << "Error: Can not yet handle zone type " << zoneType << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::ReadNodalCoordinates( const EntIndex_t currentZone, const int numNodalPointsInZone )
{
    ///Read the nodal coordinates from the current zone...
    ///If any turn out to be non-existent (e.g., planar description),
    ///then set to zero for 3D coordinates.
    vtkFloatArray* x = NULL;
    if( this->xIndex > 0 )
    {
        x = vtkFloatArray::New();
        x->SetName( "X" );
        x->SetNumberOfComponents( 1 );
        this->ReadVariable( currentZone, this->xIndex, m_varName[ this->xIndex - 1 ], x );
    }
    else
    {
        x = this->ZeroArray( "X", numNodalPointsInZone );
    }

    vtkFloatArray* y = NULL;
    if( this->yIndex > 0 )
    {
        y = vtkFloatArray::New();
        y->SetName( "Y" );
        y->SetNumberOfComponents( 1 );
        this->ReadVariable( currentZone, this->yIndex, m_varName[ this->yIndex - 1 ], y );
    }
    else
    {
        y = this->ZeroArray( "Y", numNodalPointsInZone );
    }

    vtkFloatArray* z = NULL;
    if( this->zIndex > 0 )
    {
        z = vtkFloatArray::New();
        z->SetName( "Z" );
        z->SetNumberOfComponents( 1 );
        this->ReadVariable( currentZone, this->zIndex, m_varName[ this->zIndex - 1 ], z );
    }
    else
    {
        z = this->ZeroArray( "Z", numNodalPointsInZone );
    }

    // Populate all the points to vtk...
    for( int i = 0; i < numNodalPointsInZone; ++i )
    {
        this->vertex->InsertNextPoint( x->GetValue( i ), y->GetValue( i ), z->GetValue( i ) );
    }

    x->Delete();
    y->Delete();
    z->Delete();
}

void tecplotReader::ReadNodeAndCellData( const EntIndex_t currentZone, const LgIndex_t numElementsInZone,
                                         const int numNodalPointsInZone, int parNum )
{
    int tecplotVar = 0;
    for( int i = 0; i < this->numVars; ++i )
    {
        EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
        //cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << std::endl;
        
        tecplotVar = i + 1;
        /*if( strcmp( this->m_varName[ i ], "X" ) == 0 ||
            strcmp( this->m_varName[ i ], "Y" ) == 0 ||
            strcmp( this->m_varName[ i ], "Z" ) == 0 )*/
        if( (tecplotVar == xIndex) || (tecplotVar == yIndex) || (tecplotVar == zIndex) )
        {
            // nodal coordinates already processed
        }
        else
        {
            //cout << "in ReadNodeAndCellData, parNum = " << parNum << std::endl;

            // If data is shared, read data from first zone.
            // Otherwise, read the parameter data from the current zone.
            // variable index is 1-based, names aren't
            if( dataShareCount == this->numZones )
            {
                this->ReadVariable( 1, i+1, this->m_varName[ i ], this->parameterData[ parNum ] );
            }
            else
            {
                this->ReadVariable( currentZone, i+1, this->m_varName[ i ], this->parameterData[ parNum ] );
            }
/*
            if( this->numZones > 1 && this->coordDataSharedAcrossZones )
            {
                std::string concatString( zoneName[ currentZone ] );
                concatString += this->varName[ i ];
                this->parameterData[ parNum ]->SetName( concatString.c_str() );
            }
*/
            parNum++;
        }

    } // for each variable

    if( (this->numZones > 1) && !this->coordDataSharedAcrossZones && (this->connectivityShareCount == 1) )
    {
#ifdef PRINT_HEADERS
        std::cout << "incrementing nodeOffset and elementOffset" << std::endl;
#endif // PRINT_HEADERS
        this->nodeOffset += numNodalPointsInZone;
        this->elementOffset += numElementsInZone;
        parNum = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void tecplotReader::AttachPointsAndDataToGrid()
{
    this->ugrid->SetPoints( this->vertex );
    this->vertex->Delete();

    if( numParameterArrays == 0 )
    {
        return;
    }
    //ugrid->DebugOn();

    for( int i = 0; i < this->numParameterArrays; i++ )
    {
        if( this->parameterData[ i ]->GetNumberOfTuples() == this->totalNumberOfNodalPoints )
        {
#ifdef PRINT_HEADERS
            //std::cout << "ugrid->GetPointData()->AddArray( this->parameterData[ " << i << " ] );" << std::endl;
#endif
            this->ugrid->GetPointData()->AddArray( this->parameterData[ i ] );
        }
        else if( this->parameterData[ i ]->GetNumberOfTuples() == this->totalNumberOfElements )
        {
#ifdef PRINT_HEADERS
            //std::cout << "ugrid->GetCellData()->AddArray( this->parameterData[ " << i << " ] );" << std::endl;
#endif
            vtkCellData* data = this->ugrid->GetCellData();
            //data->Print( std::cout );
            //parameterData[ i ]->Print( std::cout );
            data->AddArray( this->parameterData[ i ] );
        }
        else
        {
            std::cerr << "Error: Don't know what to do! parameterData[ " 
                << i << " ]->GetNumberOfTuples() = " 
                << this->parameterData[ i ]->GetNumberOfTuples() 
                << ", totalNumberOfNodalPoints = " << this->totalNumberOfNodalPoints 
                << ", totalNumberOfElements = " << this->totalNumberOfElements << std::endl;
        }
    }

    this->ProcessAnyVectorData( this->parameterData );

    for( int i = 0; i < this->numParameterArrays; i++ )
    {
        this->parameterData[ i ]->Delete();
    }
    delete [] this->parameterData;
    parameterData = 0;
}
////////////////////////////////////////////////////////////////////////////////
int tecplotReader::GetStartingZoneForTimestep( const int timestep )
{
    // Verify that timestep is an appropriate zero-based integer...
    if( timestep < 0 || timestep > this->numberOfTimesteps - 1 )
    {
        std::cerr << "Error: invalid request in GetStartingZoneForTimestep for file " << timestep << std::endl;
        return 0;
    }

    int sum = 0;
    for( int i = 0; i < this->numZones; i++ )
    {
        sum += this->timeToInitVtk[ i ];
        if( sum == timestep+1 )
        {
#ifdef PRINT_HEADERS
            std::cout << "StartingZoneForFile " << timestep << " is " << i+1 << std::endl;
#endif
            return( i+1 );  // zones are 1-based
        }
    }
 
    // should not get here...
    std::cerr << "Error: invalid request in GetStartingZoneForTimestep for file " << timestep << std::endl;
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int tecplotReader::GetEndingZoneForTimestep( const int timestep )
{
    // Verify that timestep is an appropriate zero-based integer...
    if( timestep < 0 || timestep > this->numberOfTimesteps - 1 )
    {
        std::cerr << "Error: invalid request in GetEndingZoneForTimestep for file " << timestep << std::endl;
        return 0;
    }

    return ( GetStartingZoneForTimestep( timestep ) + this->numZonesAtTimestep[ timestep ] - 1 );
}
////////////////////////////////////////////////////////////////////////////////
bool tecplotReader::TestForZVariable()
{
    m_varName[ 2 ] = 0;
    // variable numbers are 1-based
    TecUtilVarGetName( 3, &this->m_varName[ 2 ] ); 
    std::string zVar( this->m_varName[ 2 ] );
    std::cout << "The 3rd variable name is " << zVar << "." << std::endl;
    if( zVar[ 0 ] == 'Z' )
    {
        return true;
    }
    
    if( zVar[ zVar.size() - 1 ] == 'Z' )
    {
        return true;
    }

    if( zVar[ 0 ] == 'z' )
    {
        return true;
    }
    
    if( zVar[ zVar.size() - 1 ] == 'z' )
    {
        return true;
    }
    
    return false;
}
////////////////////////////////////////////////////////////////////////////////
