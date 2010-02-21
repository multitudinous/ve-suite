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

#include <ves/builder/DataLoader/tecplot/tecplotReader.h>

#include "TecIntegrationManager.h"
#include "ApplicationEventMonitor.h"
#include "StringList.h"

#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
//#include <vtkExtractUnstructuredGrid.h>
#include <vtkIdList.h>

#include <fstream>
#include <iostream>

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace ves::builder::DataLoader;

tecplotReader::tecplotReader( std::string inputFileNameAndPath )
{
    this->inputFileNameAndPath = inputFileNameAndPath;
    this->ugrid = NULL;
    this->numberOfOutputFiles = 0;
    this->dimension = 0;
    this->xIndex = 0;
    this->yIndex = 0;
    this->zIndex = 0;
    this->numParameterArrays = 0;
    this->totalNumberOfElements = 0;
    this->totalNumberOfNodalPoints = 0;
    this->ii = 0;
    this->nodeOffset = 0;
    this->elementOffset = 0;
    this->vertex = NULL;
    this->parameterData = NULL;

    if( ! this->tecplotIsStarted )
    {
        this->OneTimeSetup();
    }
    
    if( this->tecplotIsStarted )
    {
        std::string extension = getExtension( this->inputFileNameAndPath );
        if( extension.compare("dat") != 0 && extension.compare("tec") != 0 && extension.compare("plt") != 0 )
        {
            std::cerr << "\nWarning: Different extension than expected on input file '" << this->inputFileNameAndPath << "'.  ";
            std::cerr << "Ascthis->ii tecplot files typically have extensions '.dat' or '.tec', ";
            std::cerr << "while binary tecplot files typically with extension '.plt'." << std::endl;
        }

        if( isFileReadable( this->inputFileNameAndPath ) )
        {
            std::cout << "\nReading file '" << this->inputFileNameAndPath << "'" << std::endl;
            this->computeNumberOfOutputFiles();
            this->computeDimension();
            this->seeIfDataSharedAcrossZones();
        }
        else
        {
            std::cerr << "Error: input file does not exist or is not readable.\n" << std::endl;
        }
    }
}

tecplotReader::~tecplotReader()
{
    std::cerr << "deleting tecplotReader" << std::endl;
    if( this->ugrid )
    {
        this->ugrid->Delete();
    }

    for( int i = 0; i < this->numVars; i++ )
    {
        TecUtilStringDealloc( &this->varName[ i ] );
    }

    //std::cerr << "deleting zoneName" << std::endl;
    for( int i = 0; i < this->numZones; i++ )
    {
        //TecUtilStringDealloc( &zoneName[ i ] );
    }
}

int tecplotReader::GetNumberOfOutputFiles()
{
    return this->numberOfOutputFiles;
}

vtkUnstructuredGrid * tecplotReader::GetOutputFile( int i )
{
    if( i < 0 || i > this->numberOfOutputFiles - 1 )
    {
        std::cerr << "Error: invalid request" << std::endl;
        return NULL;
    }

    for( EntIndex_t currentZone = 1; currentZone < this->numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        this->processZone( currentZone );

        if( ( i == this->numberOfOutputFiles - 1  && this->numberOfOutputFiles == 1 && currentZone == this->numZones ) ||
            ( i == currentZone -1 && this->numberOfOutputFiles > 1 ) )
        {
            return this->ugrid;
        }
    }

    std::cerr << "Error: should not be here" << std::endl;
    return NULL;
}

void tecplotReader::OneTimeSetup()
{
    Manager::ManagerStartReturnCode_e ret;

    this->manager = new Manager();
    this->manager->setApplicationEventMonitor( new ApplicationEventMonitor() );

    char* tecSDKHomeDir = getenv("TECSDKHOME");
    if( tecSDKHomeDir )
    {
        //std::cout << "TECSDKHOME=" << tecSDKHomeDir << std::endl;
        // the following produces screen output showing tecplot version and location of your tecplot.cfg
        ret = this->manager->init( tecSDKHomeDir );
    }
    else
    {
        std::cerr << "The environment variable TECSDKHOME must be defined to run Tecplot SDK applications.\n" << std::endl;
        ret = Manager::ManagerStartReturnCode_HomeDirectoryNotSpecified;
    }

    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        // the following produces screen output showing location of tecplot executable and tecplot home dir
        ret = this->manager->start();
    }
    else
    {
        std::cerr << "Unable to initialize the this->manager\n" << std::endl;
    }

    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        TecUtilParentLockStart();

        // Tecplot starts up "pageless/frameless".
        // Create a Page, and get a Frame, which is required to load data.
        TecUtilPageCreateNew();

        this->tecplotIsStarted = 1;
    }
    else
    {
        std::cerr << "Failed to initialize the Tecplot SDK: " << this->manager->returnCodeString(ret).c_str() << std::endl;
    }

    //return ((ret == Manager::ManagerStartReturnCode_Ok) ? 0 : 1);
}

void tecplotReader::OneTimeCleanup()
{
    TecUtilParentLockFinish();
    this->manager->stop();
}

/*
vtkUnstructuredGrid * tecplotReader::GetUGrid()
{
    std::cout << "\nMerging coincident points in the unstructured grid..." << std::endl;
    vtkExtractUnstructuredGrid *extunsgrid = vtkExtractUnstructuredGrid::New();
    //extunsgrid->DebugOn();
    extunsgrid->BreakOnError();
    extunsgrid->PointClippingOn();
    extunsgrid->CellClippingOff();
    extunsgrid->ExtentClippingOff();
    extunsgrid->MergingOn();

    int numPoints = this->ugrid->GetNumberOfPoints();
    std::cout << "numPoints = " << numPoints << std::endl;
    extunsgrid->SetInput( this->ugrid );
    extunsgrid->SetPointMinimum( 0 );
    extunsgrid->SetPointMaximum( numPoints );
    extunsgrid->Update();
    this->ugrid->Delete();

    vtkUnstructuredGrid * cleanedGrid = vtkUnstructuredGrid::New();
    cleanedGrid->ShallowCopy( extunsgrid->GetOutput() );
    extunsgrid->Delete();
    return cleanedGrid;
}
*/

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

Tecplot tells us numZones & numVars. We can look at the dimension of the coordinate data, and solve,
m variables = numVars - dimension.

coordDataSharedAcrossZones == 1 means that there is 1 set of nodal coordinates, shared across all zones

if connectivityShareCount == numZones, then there is just 1 nodal connectivity array
*/

std::string tecplotReader::getExtension( const std::string& s )
{
    char sep = '.';

    size_t i = s.rfind(sep, s.length());
    if( i != std::string::npos )
    {
        return( s.substr(i+1, s.length() ) );
    }

    return( "" );
} 

int tecplotReader::isFileReadable( const std::string filename )
{
    std::ifstream fileIn( filename.c_str(), std::ios::in );
    if( ! fileIn.good() )
    {
        return 0;
    }
    fileIn.close();
    return 1;
}

void tecplotReader::readVariable( EntIndex_t currentZone, int varNumber, char * varName, vtkFloatArray *& parameterData )
{
    // Read a single variable from the current zone...
    if( varNumber )
    {
        FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef( currentZone, varNumber ); //1-based
        if( FieldData )
        {
            LgIndex_t numValues = TecUtilDataValueGetCountByRef( FieldData );

            if( parameterData == NULL )
            {
                parameterData = vtkFloatArray::New();
                parameterData->SetName( varName );
                //parameterData->SetNumberOfTuples( numValues );
                parameterData->SetNumberOfComponents( 1 );
            }

            std::cout << "reading parameter " << varNumber << " '" << varName << "' from zone " << currentZone 
                 << ", numValues = " << numValues << std::endl;
            for( int i = 0; i < numValues; i++ )
            {
                //GetByRef function is 1-based
                //parameterData->SetTuple1( i+this->nodeOffset , TecUtilDataValueGetByRef( FieldData, i+1 ) );
                parameterData->InsertNextValue( TecUtilDataValueGetByRef( FieldData, i+1 ) );
            }
        }
        else
        {
            std::cerr << "Error: Unable to read " << varName << " variable data" << std::endl;
            // will return a NULL array pointer
        }
    }
    else 
    {
        //cerr << "Error: variable number " << varNumber << " does not exist or can not be read" << std::endl;
        // will return a NULL array pointer
    }
    return;
}

vtkFloatArray * tecplotReader::zeroArray( std::string varName, int numTuples )
{
    std::cout << "setting parameter '" << varName << "' to zero" << std::endl;
    vtkFloatArray * zero = vtkFloatArray::New();
    zero->SetName( varName.c_str() );
    zero->SetNumberOfTuples( numTuples );
    zero->SetNumberOfComponents( 1 );
    for( int i = 0; i < numTuples; i++ )
    {
        zero->SetValue( i, 0.0 );
    }
    return zero;
}

void tecplotReader::readVectorNameAndUpdateIndex( int currentIndex, int currentVar, std::string s, std::string & vecName, int * vectorIndex )
{
    if( ( vectorIndex[ 0 ] + vectorIndex[ 1 ] + vectorIndex[ 2 ] ) == 0 )
    {
        vecName = s.substr( 2, s.length() );
        vectorIndex[ currentIndex ] = currentVar + 1;
    }
    else
    {
        if( vecName == s.substr( 2, s.length() ) )
        {
            vectorIndex[ currentIndex ] = currentVar + 1;
        }
        else
        {
            // reset vectorIndex 
            vectorIndex[ 0 ] = vectorIndex[ 1 ] = vectorIndex[ 2 ] = 0;
            vecName = s.substr( 2, s.length() );
            vectorIndex[ currentIndex ] = currentVar + 1;
        }
    }
    return;
}

void tecplotReader::processAnyVectorData( int numNodalPointsInZone, vtkFloatArray ** parameterData )
{
    // Now see if any variable names appear to be representing vector quantities...
    int vectorIndex[ 3 ] = { 0, 0, 0 };

    std::string vecName;

    // Look for parameters that are part of a vector quantity...
    for( int i = 0; i < this->numVars; i++ )
    {
        std::string s( this->varName[ i ] );

        // if the beginning of the variable name looks like a vector component, then...
        if( s.substr( 0, 2 ) == "X " )
        {
            readVectorNameAndUpdateIndex( 0, i, s, vecName, vectorIndex );
        }
        else if( s.substr( 0, 2 ) == "Y " )
        {
            readVectorNameAndUpdateIndex( 1, i, s, vecName, vectorIndex );
        }
        else if( s.substr( 0, 2 ) == "Z " )
        {
            readVectorNameAndUpdateIndex( 2, i, s, vecName, vectorIndex );
        }

        // when have enough information to confirm fully populated vector
        if( this->xIndex == 0 && this->yIndex == 0 && this->zIndex == 0 )
        {
            std::cerr << "Error: No coordinate data provided" << std::endl;
        }
        else if( ( vectorIndex[ 0 ] > 0 || this->xIndex == 0) &&
                 ( vectorIndex[ 1 ] > 0 || this->yIndex == 0) &&
                 ( vectorIndex[ 2 ] > 0 || this->zIndex == 0) )
        {
            std::cout << "Found vector '" << vecName << "'" << std::endl;

            vtkFloatArray * vector = vtkFloatArray::New();
            vector->SetName( vecName.c_str() );
            vector->SetNumberOfTuples( numNodalPointsInZone );
            vector->SetNumberOfComponents( 3 );

            for( int i = 0; i < numNodalPointsInZone; i++ )
            {
                for( int j = 0; j < 3; j++ )
                {
                    if( vectorIndex[ j ] == 0 )
                    {
                        vector->InsertComponent( i, j, 0.0 );
                    }
                    else
                    {
                        //cout << "copying data from " << parameterData[ vectorIndex[ j ]-1-this->dimension ]->GetName() << ", value = " << parameterData[ vectorIndex[ j ]-1-this->dimension ]->GetValue( i ) << std::endl;
                        vector->InsertComponent( i, j, parameterData[ vectorIndex[ j ]-1-this->dimension ]->GetValue( i ) );
                    }
                }
            }

            this->ugrid->GetPointData()->AddArray( vector );
            vector->Delete();

            // reset vectorIndex to look for next vector
            for( int j = 0; j < 3; j++ )
            {
                vectorIndex[ j ] = 0;
            }
        }
    }
    return;
}
 
void tecplotReader::computeNumberOfOutputFiles()
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
        NULL,                      // ZonesToRead 	Use NULL to load all zones
        VarLoadMode_ByName,        // VarLoadMode is either ByName or ByPosition
        NULL,                      // VarPositionList is used only if VarLoadMode is ByPosition. Use NULL to load all variables.
        NULL,                      // VarNameList 	 Use NULL to load only variable names common to all data files.
        1, 1, 1 );                 // Set to 1 to load every data point in the I-direction, J-direction, & K-direction. 

    char *dataset_title = NULL;
    TecUtilDataSetGetInfo( &dataset_title, &this->numZones, &this->numVars );
    std::cout << "The dataset_title is \"" << dataset_title << "\"" << std::endl;
    std::cout << "Number of zones is " << this->numZones << " and number of variables is " << this->numVars << std::endl;
    TecUtilStringDealloc( &dataset_title );

    // Is data shared across zones? (typically coordinate data or cell data)
    // Appear to get same result regardless of zone, so just look at first zone
    this->connectivityShareCount = TecUtilDataConnectGetShareCount( 1 );
    std::cout << "Connectivity share count of zone 1 is " << this->connectivityShareCount << std::endl;

    // if n zones and shared connectivity, then write vtk
    if( this->numZones > 1 && this->connectivityShareCount == this->numZones )
    {
        //cout << "writing multiple vtk files" << std::endl;
        this->numberOfOutputFiles = this->numZones;
    }
    else
    {
        //cout << "writing single vtk file" << std::endl;
        this->numberOfOutputFiles = 1;
    }
}

void tecplotReader::computeDimension()
{
    int numNonCoordinateParameters = 0;

    this->varName = new VarName_t [ this->numVars ];

    for( int i = 0; i < this->numVars; i++ )
    {
        // Read ith variable name...
        TecUtilVarGetName( i+1, &this->varName[ i ] ); // variable numbers are 1-based
        std::cout << "The name of Variable " << i+1 << " is \"" << this->varName[ i ] << "\"" << std::endl;

        // If this variable name corresponds to coordinate data, then record the 1-based index...
        if( strcmp( this->varName[ i ], "X" ) == 0 )
        {
            this->xIndex = i+1;
            this->dimension++;
        }
        else if( strcmp( this->varName[ i ], "Y" ) == 0 )
        {
            this->yIndex = i+1;
            this->dimension++;
        }
        else if( strcmp( this->varName[ i ], "Z" ) == 0 )
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
    std::cout << "dimension is " << this->dimension  << ", xIndex is " << this->xIndex << ", yIndex is " << this->yIndex << ", zIndex is " << this->zIndex << std::endl;
    std::cout << "numNonCoordinateParameters is " << numNonCoordinateParameters << std::endl;

    // Prepare to count the number of non-coordinate parameter arrays
    this->numParameterArrays = numNonCoordinateParameters;
    std::cout << "numParameterArrays = " << this->numParameterArrays << std::endl;
}

void tecplotReader::seeIfDataSharedAcrossZones()
{
    this->coordDataSharedAcrossZones = 0;

    // Is data shared across zones? (typically coordinate data or cell data)
    //for( EntIndex_t currentZone = 1; currentZone < numZones+1; currentZone++ ) // zone numbers are 1-based
    // Appear to get same result regardless of zone, so just look at first zone
    EntIndex_t currentZone = 1; // zone numbers are 1-based
    {
        for( int i = 0; i < this->numVars; i++ )
        {
            EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
            std::cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << std::endl;
            
            // If variable name corresponds to one of the coordinate labels...
            if( strcmp( this->varName[ i ], "X" ) == 0 ||
                strcmp( this->varName[ i ], "Y" ) == 0 ||
                strcmp( this->varName[ i ], "Z" ) == 0 )
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

void tecplotReader::processZone( EntIndex_t currentZone )
{
    // define flag to control printing of unique error message to a single time...
    int undefinedZoneType[ 8 ] = {1,1,1,1,1,1,1,1};

    if( ( this->numberOfOutputFiles == 1 && currentZone == 1 ) || this->numberOfOutputFiles > 1 )
    {
        // set up arrays to store scalar nodal & element data over entire mesh...
        if( this->ugrid )
        {
            this->ugrid->Delete();
            this->ugrid = NULL;
        }
        this->ugrid = vtkUnstructuredGrid::New();
        this->vertex = vtkPoints::New();
        this->parameterData = new vtkFloatArray * [ this->numParameterArrays ];
        for( int i = 0; i < this->numParameterArrays; i++ )
        {
            this->parameterData[ i ] = NULL;
        }

        if( this->numberOfOutputFiles > 1 )
        {
            //cout << "writing multiple vtk files" << std::endl;
            this->ii = 0;
        }
    }

    // For each tecplot element in current zone, read connectivity and construct corresponding VTK element
    VarName_t * zoneName = new VarName_t [ this->numZones ];

    if( TecUtilZoneGetName( currentZone, &zoneName[ currentZone ] ) )
    {
        std::cout << "For Zone " << currentZone << ", zoneName is \"" << zoneName[ currentZone ] << "\", zoneType is ";
    }
    else
    {
        std::cerr << "Error: Unable to get name of zone " << currentZone << std::endl;
    }
/*
    Set_pa MySet = TecUtilConnectGetShareZoneSet( currentZone );
    SetIndex_t Count = TecUtilSetGetMemberCount( MySet );
    for( SetIndex_t Position = 1; Position <= Count; Position++ )
    {
        SetIndex_t Member = TecUtilSetGetMember( MySet, Position );
        std::cout << "   Member is \"" << Member << "\"" << std::endl;
    }
    TecUtilSetDealloc( &MySet );

    EntIndex_t this->connectivityShareCount = TecUtilDataConnectGetShareCount( currentZone );
    std::cout << "Connectivity share count of current zone is " << this->connectivityShareCount << std::endl;
*/

    // read zoneType, numNodalPointsInZone, numElementsInZone, nodal connectivity
    // (read it again, even if it was shared and you read it already)
    ZoneType_e zoneType = TecUtilZoneGetType( currentZone );
    LgIndex_t numElementsInZone = 0;
    switch( zoneType )  // compare to those defined in GLOBAL.h
    {
        case ZoneType_Ordered:
            std::cout << "Ordered" << std::endl;
            break;
        case ZoneType_FETriangle:
            std::cout << "FETriangle" << std::endl;
            break;
        case ZoneType_FEQuad:
            std::cout << "FEQuad" << std::endl;
            break;
        case ZoneType_FETetra:
            std::cout << "FETetra" << std::endl;
            break;
        case ZoneType_FEBrick:
            std::cout << "FEBrick" << std::endl;
            break;
        case ZoneType_FELineSeg:
            std::cout << "FELineSeg" << std::endl;
            break;
        case ZoneType_FEPolygon:
            std::cout << "FEPolygon" << std::endl;
            break;
        case ZoneType_FEPolyhedron:
            std::cout << "FEPolyhedron" << std::endl;
            break;
        case END_ZoneType_e:
            std::cout << "END_ZoneType_e" << std::endl;
            break;
        case ZoneType_Invalid:
            std::cout << "Invalid" << std::endl;
            break;
        default:
            std::cout << "ZoneType not recognized. Not supposed to get here." << std::endl;
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

    int numNodesPerElement = 0;
    int numFacesPerCell = 0;
    int numNodalPointsInZone = 0;

    if( zoneType == ZoneType_Ordered )
    {
        std::cout << "   The I-dimension for ordered data is " << IMax << std::endl;
        std::cout << "   The J-dimension for ordered data is " << JMax << std::endl;
        std::cout << "   The K-dimension for ordered data is " << KMax << std::endl;
        if( IMax > 0  && JMax > 0 && KMax > 0 )
        {
            numNodesPerElement = 8;
        }
        else if( ( IMax > 0  && JMax > 0 ) ||
                 ( IMax > 0  && KMax > 0 ) || 
                 ( JMax > 0  && KMax > 0 )  )
        {
            numNodesPerElement = 4;
        }
        else if( IMax > 0 || JMax > 0 || KMax > 0 )
        {
            numNodesPerElement = 2;
        }
        else
        {
            std::cout << "IMax = JMax = KMax = 0. Not supposed to get here." << std::endl;
            numNodesPerElement = 0;
        }
    }
    else if( zoneType > ZoneType_Ordered && zoneType < END_ZoneType_e )
    {
        numNodalPointsInZone = IMax;  // tecplot called this 'number of data points' but that is misleading
        std::cout << "   The number of nodal points in this zone is " << numNodalPointsInZone << std::endl;

        numElementsInZone = JMax;
        std::cout << "   The number of elements in this zone is " << numElementsInZone << std::endl;

        if( zoneType == ZoneType_FEPolygon || zoneType == ZoneType_FEPolyhedron )
        {
            // face-based FE-data
            numFacesPerCell = KMax;
            std::cout << "   The number of faces per cell is " << numFacesPerCell << std::endl;
        }
        else
        {
            // cell-based FE-data
            numNodesPerElement = KMax;
            std::cout << "   The number of nodes per cell is " << numNodesPerElement << std::endl;
        }
    }
    else
    {
        std::cout << "ZoneType not known. Not supposed to get here." << std::endl;
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

    if( numNodesPerElement == 0 )
        std::cout << "!!! The number of nodes per element is " << numNodesPerElement << std::endl;
            
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
            //NodeMap_t * nodeArray = new NodeMap_t [ numNodesPerElement ];
            //cout << "For element " << elemNum << ", nodes =";
            for( int i = 0; i < numNodesPerElement; i++ ) 
            {
                // node numbers in tecplot are 1-based, 0-based in VTK
                //nodeArray[ i ] = TecUtilDataNodeGetByRef( nm, elemNum, i+1 ) - 1 + this->nodeOffset;
                nodeValue = TecUtilDataNodeGetByRef( nm, elemNum, i+1 ) - 1 + this->nodeOffset;
                //cout << " " << nodeArray[ i ];
                tempIdList->SetId( i, nodeValue );
            }
            //cout << std::endl;

            if( zoneType == ZoneType_FETriangle )
            {
                //this->ugrid->InsertNextCell( VTK_TRIANGLE, numNodesPerElement, nodeArray );
                this->ugrid->InsertNextCell( VTK_TRIANGLE, tempIdList );
            }
            else if( zoneType == ZoneType_FEQuad )
            {
                //this->ugrid->InsertNextCell( VTK_QUAD, numNodesPerElement, nodeArray );
                this->ugrid->InsertNextCell( VTK_QUAD, tempIdList );
            }
            else if( zoneType == ZoneType_FETetra )
            {
                //this->ugrid->InsertNextCell( VTK_TETRA, numNodesPerElement, nodeArray );
                this->ugrid->InsertNextCell( VTK_TETRA, tempIdList );
            }
            else if( zoneType == ZoneType_FEBrick )
            {
                //this->ugrid->InsertNextCell( VTK_HEXAHEDRON, numNodesPerElement, nodeArray );
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
            //delete[] nodeArray;            
        }
    }
    else
    {
        std::cerr << "Error: Unable to get node map" << std::endl;
    }

    // Read the nodal coordinates from the current zone...
    // If any turn out to be non-existent (e.g., planar description), then set to zero for 3D coordinates.
    vtkFloatArray * x = NULL;
    readVariable( currentZone, this->xIndex, "X", x );
    if( x == NULL )
    {
        x = zeroArray( "X", numNodalPointsInZone );
    }

    vtkFloatArray * y = NULL;
    readVariable( currentZone, this->yIndex, "Y", y );
    if( y == NULL )
    {
        y = zeroArray( "Y", numNodalPointsInZone );
    }

    vtkFloatArray * z = NULL;
    readVariable( currentZone, this->zIndex, "Z", z );
    if( z == NULL )
    {
        z = zeroArray( "Z", numNodalPointsInZone );
    }

    // Populate all the points to vtk...
    for( int i = 0; i < numNodalPointsInZone; i++ )
    {
        this->vertex->InsertNextPoint( x->GetValue( i ), y->GetValue( i ), z->GetValue( i ) );
    }

    x->Delete();
    y->Delete();
    z->Delete();

    for( int i = 0; i < this->numVars; i++ )
    {
        EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
        //cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << std::endl;
/*           
        Set_pa MySet = TecUtilDataValueGetShareZoneSet( currentZone, i+1 );
        SetIndex_t Count = TecUtilSetGetMemberCount( MySet );
        for( SetIndex_t Position = 1; Position <= Count; Position++ )
        {
            SetIndex_t Member = TecUtilSetGetMember( MySet, Position );
            std::cout << "Member is \"" << Member << "\"" << std::endl;
        }
        TecUtilSetDealloc( &MySet );
*/
        
        if( strcmp( this->varName[ i ], "X" ) == 0 ||
            strcmp( this->varName[ i ], "Y" ) == 0 ||
            strcmp( this->varName[ i ], "Z" ) == 0 )
        {
            // nodal coordinates already processed
        }
        else
        {
            //cout << "this->ii = " << this->ii << std::endl;

            // If data is shared, read data from first zone.
            // Otherwise, read the parameter data from the current zone.
            // variable index is 1-based, names aren't
            if( dataShareCount == this->numZones )
            {
                readVariable( 1, i+1, this->varName[ i ], this->parameterData[ this->ii ] );
            }
            else
            {
                readVariable( currentZone, i+1, this->varName[ i ], this->parameterData[ this->ii ] );
            }
/*
            if( this->numZones > 1 && this->coordDataSharedAcrossZones )
            {
                std::string concatString( zoneName[ currentZone ] );
                concatString += this->varName[ i ];
                this->parameterData[ this->ii ]->SetName( concatString.c_str() );
            }
*/
            this->ii++;
        }

    } // for each variable

    if( this->numZones > 1 && !this->coordDataSharedAcrossZones && this->connectivityShareCount == 1 )
    {
        std::cout << "incrementing this->nodeOffset and this->elementOffset" << std::endl;
        this->nodeOffset += numNodalPointsInZone;
        this->elementOffset += numElementsInZone;
        this->ii = 0;
    }

    // Is it time to create the ugrid?
    if( ( this->numberOfOutputFiles == 1 && currentZone == this->numZones ) || this->numberOfOutputFiles > 1 )
    {
        this->ugrid->SetPoints( this->vertex );
        this->vertex->Delete();

        for( int i = 0; i < this->numParameterArrays; i++ )
        {
            if( this->parameterData[ i ]->GetNumberOfTuples() == this->totalNumberOfNodalPoints )
            {
                //cout << "ugrid->GetPointData()->AddArray( this->parameterData[ " << i << " ] );" << std::endl;
                this->ugrid->GetPointData()->AddArray( this->parameterData[ i ] );
            }
            else if( this->parameterData[ i ]->GetNumberOfTuples() == this->totalNumberOfElements )
            {
                //cout << "ugrid->GetCellData()->AddArray( this->parameterData[ " << i << " ] );" << std::endl;
                this->ugrid->GetCellData()->AddArray( this->parameterData[ i ] );
            }
            else
            {
                std::cerr << "Error: Don't know what to do! this->parameterData[ " << i << " ]->GetNumberOfTuples() = " << this->parameterData[ i ]->GetNumberOfTuples() << ", this->totalNumberOfNodalPoints = " << this->totalNumberOfNodalPoints << ", this->totalNumberOfElements = " << this->totalNumberOfElements << std::endl;
            }
        }

        processAnyVectorData( numNodalPointsInZone, this->parameterData );

        for( int i = 0; i < this->numParameterArrays; i++ )
        {
            this->parameterData[ i ]->Delete();
        }
        delete [] this->parameterData;
    }
}

