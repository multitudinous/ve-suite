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

#include "TecIntegrationManager.h"
#include "Manager.h"
#include "ApplicationEventMonitor.h"
#include "StringList.h"

#include <iostream>
#include <fstream>

#include <vtkUnstructuredGrid.h>        // -lvtkFiltering
#include <vtkPoints.h>                  // -lvtkCommon
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkXMLUnstructuredGridWriter.h>  // -lvtkIO

#include <boost/lexical_cast.hpp>

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace std;

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
    n sets of nodal coordinates                              1 vtk unstructured grid with m variables
    n nodal connectivity arrays                           
    m variables in addition to nodal coordinates

Tecplot tells us numZones & numVars. We can look at the dimension of the coordinate data, and solve,
m variables = numVars - dimension.

coordDataSharedAcrossZones == 1 means that there is 1 set of nodal coordinates, shared across all zones

if connectivityShareCount == numZones, then there is just 1 nodal connectivity array
*/

int isFileReadable( const std::string filename )
{
    std::ifstream fileIn( filename.c_str(), std::ios::in );
    if( ! fileIn.good() )
    {
        return 0;
    }
    fileIn.close();
    return 1;
}

string extractFileNameFromFullPath( const string& s )
{
    char sep = '/';

#ifdef WIN32
    sep = '\';
#endif

    size_t i = s.rfind(sep, s.length());
    if( i != string::npos )
    {
        return( s.substr(i+1, s.length() - i) );
    }

    return( s );
} 

string stripExtension( const string& s )
{
    char sep = '.';

    size_t i = s.rfind(sep, s.length());
    if( i != string::npos )
    {
        return( s.substr(0, i ) );
    }

    return( s );
} 

string getExtension( const string& s )
{
    char sep = '.';

    size_t i = s.rfind(sep, s.length());
    if( i != string::npos )
    {
        return( s.substr(i+1, s.length() ) );
    }

    return( "" );
} 

vtkFloatArray * readVariable( EntIndex_t currentZone, int varNumber, char * varName, int nodeOffset )
{
    vtkFloatArray * parameterData = NULL;

    // Read a single variable from the current zone...
    if( varNumber )
    {
        FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef( currentZone, varNumber ); //1-based
        if( FieldData )
        {
            LgIndex_t numValues = TecUtilDataValueGetCountByRef( FieldData );

            parameterData = vtkFloatArray::New();
            parameterData->SetName( varName );
            parameterData->SetNumberOfTuples( numValues );
            parameterData->SetNumberOfComponents( 1 );

            cout << "reading parameter " << varNumber << " '" << varName << "' from zone " << currentZone 
                 << ", numValues = " << numValues << endl;
            for( int i = 0; i < numValues; i++ )
            {
                //GetByRef function is 1-based
                parameterData->SetTuple1( i+nodeOffset, TecUtilDataValueGetByRef( FieldData, i+1 ) );
            }
        }
        else
        {
            cerr << "Error: Unable to read " << varName << " variable data" << endl;
            // will return a NULL array pointer
        }
    }
    else 
    {
        //cerr << "Error: variable number " << varNumber << " does not exist or can not be read" << endl;
        // will return a NULL array pointer
    }
    return parameterData;
}
vtkFloatArray * zeroArray( string varName, int numTuples )
{
    cout << "setting parameter '" << varName << "' to zero" << endl;
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

void readVectorNameAndUpdateIndex( int currentIndex, int currentVar, string s, string & vecName, int * vectorIndex )
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
 
void convertTecplotToVTK( string inputFileNameAndPath )
{
    StringList fileName( inputFileNameAndPath.c_str(), NULL );

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
    EntIndex_t numZones, numVars;
    TecUtilDataSetGetInfo( &dataset_title, &numZones, &numVars );
    cout << "The dataset_title is \"" << dataset_title << "\"" << endl;
    cout << "Number of zones is " << numZones << " and number of variables is " << numVars << endl;
    TecUtilStringDealloc( &dataset_title );

    VarName_t * varName = new VarName_t [ numVars ];
    int xIndex = 0;
    int yIndex = 0;
    int zIndex = 0;
    int dimension = 0;  //determine whether original tecplot data uses 1d, 2d, or 3d coordinates
    int numNonCoordinateParameters = 0;

    for( int i = 0; i < numVars; i++ )
    {
        // Read ith variable name...
        TecUtilVarGetName( i+1, &varName[ i ] ); // variable numbers are 1-based
        cout << "The name of Variable " << i+1 << " is \"" << varName[ i ] << "\"" << endl;

        // If this variable name corresponds to coordinate data, then record the 1-based index...
        if( strcmp( varName[ i ], "X" ) == 0 )
        {
            xIndex = i+1;
            dimension++;
        }
        else if( strcmp( varName[ i ], "Y" ) == 0 )
        {
            yIndex = i+1;
            dimension++;
        }
        else if( strcmp( varName[ i ], "Z" ) == 0 )
        {
            zIndex = i+1;
            dimension++;
        }
        else
        {
            // count number of non-coordinate nodal data parameters
            numNonCoordinateParameters++;
        }
    }

    cout << "dimension is " << dimension  << ", xIndex is " << xIndex << ", yIndex is " << yIndex << ", zIndex is " << zIndex << endl;
    cout << "numNonCoordinateParameters is " << numNonCoordinateParameters << endl;

    // Prepare to count the number of non-coordinate parameter arrays
    int numParameterArrays = numNonCoordinateParameters;

    int coordDataSharedAcrossZones = 0;
    EntIndex_t connectivityShareCount;

    // Is data shared across zones? (typically coordinate data or cell data)
    //for( EntIndex_t currentZone = 1; currentZone < numZones+1; currentZone++ ) // zone numbers are 1-based
    // Appear to get same result regardless of zone, so just look at first zone
    EntIndex_t currentZone = 1; // zone numbers are 1-based
    {
        connectivityShareCount = TecUtilDataConnectGetShareCount( currentZone );

        for( int i = 0; i < numVars; i++ )
        {
            EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
            cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << endl;
            
            // If variable name corresponds to one of the coordinate labels...
            if( strcmp( varName[ i ], "X" ) == 0 ||
                strcmp( varName[ i ], "Y" ) == 0 ||
                strcmp( varName[ i ], "Z" ) == 0 )
            {
                if( dataShareCount == numZones )
                {
                    // coordinate data is shared across zones
                    coordDataSharedAcrossZones = 1;
                    //numParameterArrays = numNonCoordinateParameters * numZones;
                }
                else
                {
                    // coordinate data is specified for each zone
                    //numParameterArrays = numNonCoordinateParameters;
                }
            }
        }
    }

    cout << "Connectivity share count of zone 1 is " << connectivityShareCount << endl;
    cout << "numParameterArrays = " << numParameterArrays << endl;

    // set up arrays to store scalar nodal & element data over entire mesh...
    vtkFloatArray ** parameterData = NULL;
    vtkUnstructuredGrid *ugrid = NULL;
    vtkPoints *vertex = NULL;

    // if n zones and shared connectivity, then write vtk
    if( numZones > 1 && connectivityShareCount == numZones )
    {
        //cout << "writing multiple vtk files" << endl;
    }
    else
    {
        //cout << "writing single vtk file" << endl;
        ugrid = vtkUnstructuredGrid::New();
        vertex = vtkPoints::New();
        parameterData = new vtkFloatArray * [ numParameterArrays ];
    }

    int ii = 0;

    int nodeOffset = 0;
    int numNodalPoints = 0;
    LgIndex_t numElements = 0;
    VarName_t * zoneName = new VarName_t [ numZones ];

    // define flag to control printing of unique error message to a single time...
    int undefinedZoneType[ 8 ] = {1,1,1,1,1,1,1,1};

    for( EntIndex_t currentZone = 1; currentZone < numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        if( numZones > 1 && connectivityShareCount == numZones )
        {
            //cout << "writing multiple vtk files" << endl;
            ugrid = vtkUnstructuredGrid::New();
            vertex = vtkPoints::New();
            parameterData = new vtkFloatArray * [ numParameterArrays ];
            ii = 0;
        }
        else
        {
            //cout << "writing single vtk file" << endl;
        }

        if( TecUtilZoneGetName( currentZone, &zoneName[ currentZone ] ) )
        {
            cout << "For Zone " << currentZone << ", zoneName is \"" << zoneName[ currentZone ] << "\", zoneType is ";
        }
        else
        {
            cerr << "Error: Unable to get name of zone " << currentZone << endl;
        }
/*
        Set_pa MySet = TecUtilConnectGetShareZoneSet( currentZone );
        SetIndex_t Count = TecUtilSetGetMemberCount( MySet );
        for( SetIndex_t Position = 1; Position <= Count; Position++ )
        {
            SetIndex_t Member = TecUtilSetGetMember( MySet, Position );
            cout << "   Member is \"" << Member << "\"" << endl;
        }
        TecUtilSetDealloc( &MySet );

        EntIndex_t connectivityShareCount = TecUtilDataConnectGetShareCount( currentZone );
        cout << "Connectivity share count of current zone is " << connectivityShareCount << endl;
*/

        // read zoneType, numNodalPoints, numElements, nodal connectivity
        // (read it again, even if it was shared and you read it already)
        ZoneType_e zoneType = TecUtilZoneGetType( currentZone );
        switch( zoneType )  // compare to those defined in GLOBAL.h
        {
            case ZoneType_Ordered:
                cout << "Ordered" << endl;
                break;
            case ZoneType_FETriangle:
                cout << "FETriangle" << endl;
                break;
            case ZoneType_FEQuad:
                cout << "FEQuad" << endl;
                break;
            case ZoneType_FETetra:
                cout << "FETetra" << endl;
                break;
            case ZoneType_FEBrick:
                cout << "FEBrick" << endl;
                break;
            case ZoneType_FELineSeg:
                cout << "FELineSeg" << endl;
                break;
            case ZoneType_FEPolygon:
                cout << "FEPolygon" << endl;
                break;
            case ZoneType_FEPolyhedron:
                cout << "FEPolyhedron" << endl;
                break;
            case END_ZoneType_e:
                cout << "END_ZoneType_e" << endl;
                break;
            case ZoneType_Invalid:
                cout << "Invalid" << endl;
                break;
            default:
                cout << "ZoneType not recognized. Not supposed to get here." << endl;
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

        if( zoneType == ZoneType_Ordered )
        {
            cout << "   The I-dimension for ordered data is " << IMax << endl;
            cout << "   The J-dimension for ordered data is " << JMax << endl;
            cout << "   The K-dimension for ordered data is " << KMax << endl;
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
                cout << "IMax = JMax = KMax = 0. Not supposed to get here." << endl;
                numNodesPerElement = 0;
            }
        }
        else if( zoneType > ZoneType_Ordered && zoneType < END_ZoneType_e )
        {
            numNodalPoints = IMax;  // tecplot called this 'number of data points' but that is misleading
            cout << "   The number of nodal points is " << numNodalPoints << endl;

            numElements = JMax;
            cout << "   The number of elements is " << numElements << endl;

            if( zoneType == ZoneType_FEPolygon || zoneType == ZoneType_FEPolyhedron )
            {
                // face-based FE-data
                numFacesPerCell = KMax;
                cout << "   The number of faces per cell is " << numFacesPerCell << endl;
            }
            else
            {
                // cell-based FE-data
                numNodesPerElement = KMax;
                cout << "   The number of nodes per cell is " << numNodesPerElement << endl;
            }
        }
        else
        {
            cout << "ZoneType not known. Not supposed to get here." << endl;
        }

        if( numNodesPerElement == 0 )
            cout << "!!! The number of nodes per element is " << numNodesPerElement << endl;

        // For each tecplot element in current zone, read connectivity and construct corresponding VTK element
        NodeMap_pa nm = TecUtilDataNodeGetReadableRef( currentZone );
        if( nm )
        {
            for( LgIndex_t elemNum = 1; elemNum < numElements+1; elemNum++ ) // element numbers are 1-based
            {
                // Node information (connectivity)
                // NOTE - You could use the "RawPtr" functions if speed is a critical issue
                NodeMap_t nodeArray[ numNodesPerElement ];

                //cout << "For element " << elemNum << ", nodes =";
                for( int i = 0; i < numNodesPerElement; i++ ) 
                {
                    // node numbers in tecplot are 1-based, 0-based in VTK
                    nodeArray[ i ] = TecUtilDataNodeGetByRef( nm, elemNum, i+1 ) - 1 + nodeOffset;
                    //cout << " " << nodeArray[ i ];
                }
                //cout << endl;

                if( zoneType == ZoneType_FETriangle )
                {
                    ugrid->InsertNextCell( VTK_TRIANGLE, numNodesPerElement, nodeArray );
                }
                else if( zoneType == ZoneType_FEQuad )
                {
                    ugrid->InsertNextCell( VTK_QUAD, numNodesPerElement, nodeArray );
                }
                else if( zoneType == ZoneType_FETetra )
                {
                    ugrid->InsertNextCell( VTK_TETRA, numNodesPerElement, nodeArray );
                }
                else if( zoneType == ZoneType_FEBrick )
                {
                    ugrid->InsertNextCell( VTK_HEXAHEDRON, numNodesPerElement, nodeArray );
                }
                else
                {
                    if( undefinedZoneType[ zoneType ] )
                    {
                        cerr << "Error: Can not yet handle element type " << zoneType
                             << ", numNodesPerElement = " << numNodesPerElement << endl;
                        undefinedZoneType[ zoneType ] = 0;  // set flag so as to not print more than one error msg
                    }

                    ugrid->InsertNextCell( VTK_EMPTY_CELL, 0, NULL );
                }
            }
        }
        else
        {
            cerr << "Error: Unable to get node map" << endl;
        }

        // Read the nodal coordinates from the current zone...
        // If any turn out to be non-existent (e.g., planar description), then set to zero for 3D coordinates.
        vtkFloatArray * x = readVariable( currentZone, xIndex, "X", 0 );
        if( x == 0 )
        {
            x = zeroArray( "X", numNodalPoints );
        }

        vtkFloatArray * y = readVariable( currentZone, yIndex, "Y", 0 );
        if( y == 0 )
        {
            y = zeroArray( "Y", numNodalPoints );
        }

        vtkFloatArray * z = readVariable( currentZone, zIndex, "Z", 0 );
        if( z == 0 )
        {
            z = zeroArray( "Z", numNodalPoints );
        }

        // Populate all the points to vtk...
        for( int i = 0; i < numNodalPoints; i++ )
        {
            vertex->InsertNextPoint( x->GetValue( i ), y->GetValue( i ), z->GetValue( i ) );
        }

        x->Delete();
        y->Delete();
        z->Delete();

        for( int i = 0; i < numVars; i++ )
        {
            EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
            //cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << endl;
/*           
            Set_pa MySet = TecUtilDataValueGetShareZoneSet( currentZone, i+1 );
            SetIndex_t Count = TecUtilSetGetMemberCount( MySet );
            for( SetIndex_t Position = 1; Position <= Count; Position++ )
            {
                SetIndex_t Member = TecUtilSetGetMember( MySet, Position );
                cout << "Member is \"" << Member << "\"" << endl;
            }
            TecUtilSetDealloc( &MySet );
*/
            
            if( strcmp( varName[ i ], "X" ) == 0 ||
                strcmp( varName[ i ], "Y" ) == 0 ||
                strcmp( varName[ i ], "Z" ) == 0 )
            {
                // nodal coordinates already processed
            }
            else
            {
                //cout << "ii = " << ii << endl;

                // If data is shared, read data from first zone.
                // Otherwise, read the parameter data from the current zone.
                // variable index is 1-based, names aren't
                if( dataShareCount == numZones )
                {
                    parameterData[ ii ] = readVariable( 1, i+1, varName[ i ], nodeOffset );
                }
                else
                {
                    parameterData[ ii ] = readVariable( currentZone, i+1, varName[ i ], nodeOffset );
                }
/*
                if( numZones > 1 && coordDataSharedAcrossZones )
                {
                    string concatString( zoneName[ currentZone ] );
                    concatString += varName[ i ];
                    parameterData[ ii ]->SetName( concatString.c_str() );
                }
*/
                ii++;
            }

        } // for each variable

        if( numZones > 1 && !coordDataSharedAcrossZones && connectivityShareCount == 1 )
        {
            cout << "incrementing nodeOffset" << endl;
            nodeOffset += numNodalPoints;
            ii = 0;
        }

        // if n zones and shared connectivity, then write vtk
        if( numZones > 1 && connectivityShareCount == numZones )
        {
            //cout << "writing multiple vtk files" << endl;
            ugrid->SetPoints( vertex );
            vertex->Delete();

            for( int i = 0; i < numParameterArrays; i++ )
            {
                if( parameterData[ i ]->GetNumberOfTuples() == numNodalPoints )
                {
                    //cout << "ugrid->GetPointData()->AddArray( parameterData[ " << i << " ] );" << endl;
                    ugrid->GetPointData()->AddArray( parameterData[ i ] );
                }
                else if( parameterData[ i ]->GetNumberOfTuples() == numElements )
                {
                    //cout << "ugrid->GetCellData()->AddArray( parameterData[ " << i << " ] );" << endl;
                    ugrid->GetCellData()->AddArray( parameterData[ i ] );
                }
                else
                {
                    cerr << "Error: Don't know what to do! parameterData[ " << i << " ]->GetNumberOfTuples() = " << parameterData[ i ]->GetNumberOfTuples() << endl;
                }
            }

            // Now see if any variable names appear to be representing vector quantities...
            int vectorIndex[ 3 ] = { 0, 0, 0 };

            string vecName;

            // Look for parameters that are part of a vector quantity...
            for( int i = 0; i < numVars; i++ )
            {
                string s( varName[ i ] );

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
                if( xIndex == 0 && yIndex == 0 && zIndex == 0 )
                {
                    cerr << "Error: No coordinate data provided" << endl;
                }
                else if( ( vectorIndex[ 0 ] > 0 || xIndex == 0) &&
                         ( vectorIndex[ 1 ] > 0 || yIndex == 0) &&
                         ( vectorIndex[ 2 ] > 0 || zIndex == 0) )
                {
                    cout << "Found vector '" << vecName << "'" << endl;

                    vtkFloatArray * vector = vtkFloatArray::New();
                    vector->SetName( vecName.c_str() );
                    vector->SetNumberOfTuples( numNodalPoints );
                    vector->SetNumberOfComponents( 3 );

                    for( int i = 0; i < numNodalPoints; i++ )
                    {
                        for( int j = 0; j < 3; j++ )
                        {
                            if( vectorIndex[ j ] == 0 )
                            {
                                vector->InsertComponent( i, j, 0.0 );
                            }
                            else
                            {
                                //cout << "copying data from " << parameterData[ vectorIndex[ j ]-1-dimension ]->GetName() << ", value = " << parameterData[ vectorIndex[ j ]-1-dimension ]->GetValue( i ) << endl;
                                vector->InsertComponent( i, j, parameterData[ vectorIndex[ j ]-1-dimension ]->GetValue( i ) );
                            }
                        }
                    }

                    ugrid->GetPointData()->AddArray( vector );
                    vector->Delete();

                    // reset vectorIndex to look for next vector
                    for( int j = 0; j < 3; j++ )
                    {
                        vectorIndex[ j ] = 0;
                    }
                }
            }

            for( int i = 0; i < numParameterArrays; i++ )
            {
                parameterData[ i ]->Delete();
            }
            delete [] parameterData;

            // Using a zero-based incremental naming scheme, create a *.vtu output filename to be written to current location...
            // Use boost for number-to-string conversion:
            string outputFileName = stripExtension( extractFileNameFromFullPath( inputFileNameAndPath ) ) 
                                    + "-" + boost::lexical_cast<string>(currentZone-1) + ".vtu";
            cout << "Writing to file \"" << outputFileName << "\"\n" << endl;

            vtkXMLUnstructuredGridWriter *writer = vtkXMLUnstructuredGridWriter::New();
            writer->SetInput( ugrid );
            writer->SetFileName( outputFileName.c_str() );
            writer->SetDataModeToAscii();
            writer->Write();
            writer->Delete();

            ugrid->Delete();
        }
        else
        {
            //cout << "writing single vtk file" << endl;
        }

    } // for each zone

    // if n zones and shared connectivity, then write vtk
    if( numZones > 1 && connectivityShareCount == numZones )
    {
        //cout << "writing multiple vtk files" << endl;
    }
    else
    {
        //cout << "writing single vtk file" << endl;
        //cout << "ugrid->SetPoints( vertex );" << endl;
        ugrid->SetPoints( vertex );
        vertex->Delete();

        for( int i = 0; i < numParameterArrays; i++ )
        {
            if( parameterData[ i ]->GetNumberOfTuples() == numNodalPoints )
            {
                //cout << "ugrid->GetPointData()->AddArray( parameterData[ " << i << " ] );" << endl;
                ugrid->GetPointData()->AddArray( parameterData[ i ] );
            }
            else if( parameterData[ i ]->GetNumberOfTuples() == numElements )
            {
                //cout << "ugrid->GetCellData()->AddArray( parameterData[ " << i << " ] );" << endl;
                ugrid->GetCellData()->AddArray( parameterData[ i ] );
            }
            else
            {
                cerr << "Error: Don't know what to do! parameterData[ " << i << " ]->GetNumberOfTuples() = " << parameterData[ i ]->GetNumberOfTuples() << endl;
            }
            parameterData[ i ]->Delete();
        }
        delete [] parameterData;

        // create a *.vtu output filename to be written to current location...
        string outputFileName = stripExtension( extractFileNameFromFullPath( inputFileNameAndPath ) ) + ".vtu";
        cout << "Writing to file \"" << outputFileName << "\"\n" << endl;

        vtkXMLUnstructuredGridWriter *writer = vtkXMLUnstructuredGridWriter::New();
        writer->SetInput( ugrid );
        writer->SetFileName( outputFileName.c_str() );
        writer->SetDataModeToAscii();
        writer->Write();
        writer->Delete();

        ugrid->Delete();
    }

    for( int i = 0; i < numVars; i++ )
    {
        TecUtilStringDealloc( &varName[ i ] );
    }

    //cerr << "deleting zoneName" << endl;
    for( int i = 0; i < numZones; i++ )
    {
        //TecUtilStringDealloc( &zoneName[ i ] );
    }
    //cerr << "returning to main" << endl;
}

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        cout << "Need at least one argument specifying a tecplot filename!" << endl;
        cout << "Usage: " << argv[ 0 ] << " file1 file2 ..." << endl;
        cout << "Note: If get segmentation fault right away, verify that Tecplot SDK evaluation license" << endl;
        cout << "      file 'sdkeval.lic' is at location specified by environment variable TECSDKHOME.\n" << endl;
        return( 1 );
    }
/*
    // A bit ugly here but force the inclusion of the -b flag into the program arguments.
    // This will guarantee that this will run in batch mode.
    char **FinalArgV = (char **)malloc(sizeof(char *) * (argc + 1));
    memcpy((void*)FinalArgV, (void*)argv, argc * sizeof(char*));
    static char *batchFlag = "-b";
    FinalArgV[argc] = batchFlag;
*/
    Manager* manager = new Manager();
    manager->setApplicationEventMonitor(new ApplicationEventMonitor());

    char* tecSDKHomeDir = getenv("TECSDKHOME");

    Manager::ManagerStartReturnCode_e ret;
    if( tecSDKHomeDir )
    {
        //fprintf(stdout,"TECSDKHOME=%s\n",tecSDKHomeDir); fflush(stdout);
        ret = manager->init(tecSDKHomeDir);
	    //ret = manager->init(argc + 1, FinalArgV, tecSDKHomeDir);
    }
    else
    {
        fprintf(stderr, "The environment variable TECSDKHOME must be defined to run Tecplot SDK applications.\n");
        ret = Manager::ManagerStartReturnCode_HomeDirectoryNotSpecified;
    }

    //Start Tecplot
    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        ret = manager->start();
    }
    else
    {
        fprintf(stderr, "Unable to initialize the manager\n");
    }

    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        TecUtilParentLockStart();

        // Tecplot starts up "pageless/frameless".  Be creating a Page, we get
        // a Frame, which is required to load data.
        TecUtilPageCreateNew();

        for( int i = 1; i < argc; i++ ) // argument array is 0-based, but we won't use the zeroth one (program name)
        {
            string inputFileNameAndPath( argv[ i ] );

            string extension = getExtension( inputFileNameAndPath );
            if( extension.compare("dat") != 0 && extension.compare("tec") != 0 && extension.compare("plt") != 0 )
            {
                cerr << "\nWarning: Different extension than expected on input file '" << inputFileNameAndPath << "'.  ";
                cerr << "Ascii tecplot files typically have extensions '.dat' or '.tec', ";
                cerr << "while binary tecplot files typically with extension '.plt'." << endl;
            }

            if( isFileReadable( inputFileNameAndPath ) )
            {
                cout << "\nReading file '" << inputFileNameAndPath << "'" << endl;
                convertTecplotToVTK( inputFileNameAndPath );
            }
            else
            {
                cerr << "Error: input file does not exist or is not readable.\n" << endl;
            }
        }

        TecUtilParentLockFinish();

        if( ret == Manager::ManagerStartReturnCode_Ok )
            manager->stop();
        else
            fprintf(stderr, "Tecplot SDK Startup Error: %d\n", ret);
    }
    else
    {
        fprintf( stderr, "Failed to initialize the Tecplot SDK: %s\n", manager->returnCodeString(ret).c_str() );
    }

    //free(FinalArgV);
    return ((ret == Manager::ManagerStartReturnCode_Ok) ? 0 : 1);
}


