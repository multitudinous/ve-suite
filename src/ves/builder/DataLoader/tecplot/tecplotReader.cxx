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

#include <vtkUnstructuredGrid.h>        // -lvtkFiltering
#include <vtkPoints.h>                  // -lvtkCommon
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkUnstructuredGridWriter.h>  // -lvtkIO

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace std;

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

    return( "" );
} 

string replaceExtension( const string& s, const string& newextension )
{
    char sep = '.';

    size_t i = s.rfind(sep, s.length());
    if( i != string::npos )
    {
        return( s.substr(0, i+1 ) + newextension );
    }

    return( "" );
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

double * readVariable( EntIndex_t currentZone, int varNumber, char * varName )
{
    double * array = NULL;

    // Read a single variable from the current zone...
    if( varNumber )
    {
        FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef( currentZone, varNumber ); //1-based
        if( FieldData )
        {
            LgIndex_t NumValues = TecUtilDataValueGetCountByRef( FieldData );
            array = new double [ NumValues ];

/*
            if( NumValues != numDataPoints )
            {
                cout << "NumValues != numDataPoints" << endl;
            }
*/
            cout << "   Reading Variable " << varName << endl;
            for( int i = 0; i < NumValues; i++ )
            {
                array[ i ] = TecUtilDataValueGetByRef( FieldData, i+1 ); //GetByRef function is 1-based
                //cout << "   " << varName << "[" << i+1 << "] = " << array[ i ] << endl;
            }
        }
        else
        {
            cerr << "Error: Unable to read " << varName << " variable data" << endl;
        }
    }
    else    // varNumber is zero which means it does not exist
    {
        // will return a NULL array pointer
    }
    return array;
}

void convertTecplotToVTK( string inputFileNameAndPath )
{
    StringList fileName(inputFileNameAndPath.c_str(), NULL);

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
    TecUtilDataSetGetInfo(&dataset_title, &numZones, &numVars);
    cout << "The dataset_title is \"" << dataset_title << "\"" << endl;
    cout << "Number of zones is " << numZones << " and number of variables is " << numVars << endl;
    TecUtilStringDealloc(&dataset_title);

    VarName_t * varName = new VarName_t [ numVars ];
    int xIndex = 0;
    int yIndex = 0;
    int zIndex = 0;
    int numNonCoordNodalParameters = 0;

    for( int i = 0; i < numVars; i++ )
    {
        TecUtilVarGetName( i+1, &varName[ i ] ); // variable numbers are 1-based
        cout << "The name of Variable " << i+1 << " is \"" << varName[ i ] << "\"" << endl;
        if( strcmp( varName[ i ], "X" ) == 0 )
        {
            xIndex = i+1;
        }
        else if( strcmp( varName[ i ], "Y" ) == 0 )
        {
            yIndex = i+1;
        }
        else if( strcmp( varName[ i ], "Z" ) == 0 )
        {
            zIndex = i+1;
        }
        else
        {
            // count number of non-coordinate nodal data parameters
            numNonCoordNodalParameters++;
        }
    }

    //cout << "xIndex is " << xIndex << ", yIndex is " << yIndex << ", zIndex is " << zIndex << endl;
    cout << "numNonCoordNodalParameters is " << numNonCoordNodalParameters << endl;

    // Is data shared across zones (usually coordinate data)
    int numParameterArrays = 0;
    for( EntIndex_t currentZone = 1; currentZone < numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        for( int i = 0; i < numVars; i++ )
        {
            EntIndex_t dataShareCount = TecUtilDataValueGetShareCount( currentZone, i+1 );
            //cout << "for var " << i+1 << ", dataShareCount is " << dataShareCount << endl;
            
            // See if any coordinate data is shared across zones
            if( strcmp( varName[ i ], "X" ) == 0 ||
                strcmp( varName[ i ], "Y" ) == 0 ||
                strcmp( varName[ i ], "Z" ) == 0 )
            {
                if( dataShareCount == numZones )
                {
                    // coordinate data is shared across zones
                    numParameterArrays = numNonCoordNodalParameters * numZones;
                }
                else
                {
                    numParameterArrays = numNonCoordNodalParameters;
                }
            }
        }
    }

    cout << "numParameterArrays = " << numParameterArrays << endl;
    // set up arrays to store scalar nodal data over entire mesh...
    vtkFloatArray ** parameterData = new vtkFloatArray * [ numParameterArrays ];

    vtkUnstructuredGrid *ugrid = vtkUnstructuredGrid::New();
    vtkPoints *vertex = vtkPoints::New();

    bool shouldReadCoords = true;
    int ii = 0;

    int nodeOffset = 0;
    int numDataPoints = 0;
    VarName_t * zoneName = new VarName_t [ numZones ];

    for( EntIndex_t currentZone = 1; currentZone < numZones+1; currentZone++ ) // zone numbers are 1-based
    {
        if( TecUtilZoneGetName( currentZone, &zoneName[ currentZone ] ) )
        {
            cout << "For Zone " << currentZone << ", zoneName is \"" << zoneName[ currentZone ] << "\" and zoneType is ";
        }
/*
        Set_pa MySet = TecUtilConnectGetShareZoneSet( currentZone );
        SetIndex_t Count = TecUtilSetGetMemberCount( MySet );
        for( SetIndex_t Position = 1; Position <= Count; Position++ )
        {
            SetIndex_t Member = TecUtilSetGetMember( MySet, Position );
            cout << "Member is \"" << Member << "\"" << endl;
        }
        TecUtilSetDealloc( &MySet );
        
        EntIndex_t shareCount = TecUtilDataConnectGetShareCount( currentZone );
        cout << "shareCount is " << shareCount << endl;
*/
        if( shouldReadCoords )
        {
            numDataPoints = 0;
            ZoneType_e zoneType = TecUtilZoneGetType( currentZone );
            switch( zoneType )
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
            LgIndex_t numElements = 0;

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
                numDataPoints = IMax;
                cout << "   The number of data points is " << numDataPoints << endl;

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

                    if( zoneType == ZoneType_FEBrick )
                    {
                        ugrid->InsertNextCell( VTK_HEXAHEDRON, numNodesPerElement, nodeArray );
                    }
                    else if( zoneType == ZoneType_FETriangle )
                    {
                        ugrid->InsertNextCell( VTK_TRIANGLE, numNodesPerElement, nodeArray );
                    }
                    else if( zoneType == ZoneType_FEQuad )
                    {
                        ugrid->InsertNextCell( VTK_QUAD, numNodesPerElement, nodeArray );
                    }
                    else
                    {
                        cout << "Note: Can not yet handle element type " << zoneType
                             << ", numNodesPerElement = " << numNodesPerElement << endl;

                        ugrid->InsertNextCell( VTK_EMPTY_CELL, 0, NULL );
                    }
                }
            }
            else
            {
                cerr << "Error: Unable to get node map" << endl;
            }

            // Read the nodal coordinates from the current zone...
            double * x = readVariable( currentZone, xIndex, "X" );
            if( x == 0 )
            {
                x = new double [ numDataPoints ];
                for( int i = 0; i < numDataPoints; i++ )
                    x[ i ] = 0.0;
            }

            double * y = readVariable( currentZone, yIndex, "Y" );
            if( y == 0 )
            {
                y = new double [ numDataPoints ];
                for( int i = 0; i < numDataPoints; i++ )
                    y[ i ] = 0.0;
            }

            double * z = readVariable( currentZone, zIndex, "Z" );
            if( z == 0 )
            {
                z = new double [ numDataPoints ];
                for( int i = 0; i < numDataPoints; i++ )
                    z[ i ] = 0.0;
            }

            //cout << "vertex->InsertNextPoint" << endl;
            for( int i = 0; i < numDataPoints; i++ )
            {
                vertex->InsertNextPoint( x[ i ], y[ i ], z[ i ] );
            }

            delete [] x;
            delete [] y;
            delete [] z;

        } // shouldReadCoords

        double * tempArray;
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

                if( dataShareCount == numZones )
                {
                    //set flag to prevent reading redundant nodal coordinates in future
                    shouldReadCoords = false;
                }
            }
            else
            {
                // Read the parameter data from the current zone...
                tempArray = readVariable( currentZone, i+1, varName[ i ] );    //variable index is 1-based, names aren't
/*
                cout << "ugrid->GetPointData()->GetNumberOfArrays() = " << ugrid->GetPointData()->GetNumberOfArrays() << endl;
                cout << "ii = " << ii << endl;
                cout << "numDataPoints = " << numDataPoints << endl;
*/
                parameterData[ ii ] = vtkFloatArray::New();
                parameterData[ ii ]->SetNumberOfTuples( numDataPoints );

                //dirStringStream << "displacement mag " << currentDataSetSolution << " " << displacementUnits;
                if( ! shouldReadCoords && numZones > 1 )
                {
                    parameterData[ ii ]->SetName( zoneName[ currentZone ] ); //dirStringStream.str().c_str() );
                }
                else
                {
                    parameterData[ ii ]->SetName( varName[ i ] ); //dirStringStream.str().c_str() );
                }

                parameterData[ ii ]->SetNumberOfComponents( 1 );
                //dirStringStream.str( "" );

                for( int j = 0; j < numDataPoints; j++ )
                {
                    //cout << "parameterData[ " << ii << " ]->SetTuple1( " << j+nodeOffset << ", tempArray[ " << j << " ] );" << endl;
                    parameterData[ ii ]->SetTuple1( j+nodeOffset, tempArray[ j ] );
                }
                delete [] tempArray;

                ii++;
            }

        } // for each variable

        if( shouldReadCoords )
        {
            nodeOffset += numDataPoints;
            ii = 0;
        }

    } // for each zone

    //cout << "ugrid->SetPoints( vertex );" << endl;
    ugrid->SetPoints( vertex );
    vertex->Delete();

    for( int i = 0; i < numParameterArrays; i++ )
    {
        //cout << "ugrid->GetPointData()->AddArray( parameterData[ " << i << " ] );" << endl;
        ugrid->GetPointData()->AddArray( parameterData[ i ] );
        parameterData[ i ]->Delete();
    }
    delete [] parameterData;

    // create a *.vt *.vtkk output filename to be written to current location...
    string outputFileName = replaceExtension( extractFileNameFromFullPath( inputFileNameAndPath ), "vtk" );
    cout << "Writing to file \"" << outputFileName << "\"" << endl;

    vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
    writer->SetInput( ugrid );
    writer->SetFileType( VTK_ASCII );
    writer->SetFileName( outputFileName.c_str() );
    writer->Write();
    writer->Delete();

    ugrid->Delete();

    for( int i = 0; i < numVars; i++ )
    {
        TecUtilStringDealloc( &varName[ i ] );
    }

    for( int i = 0; i < numZones; i++ )
    {
        //TecUtilStringDealloc( &zoneName[ i ] );
    }

}

int main( int argc, char** argv )
{
    if( argc < 2 )
    {
        cout << "Need at least one argument specifying a tecplot filename!" << endl;
        cout << "Usage: " << argv[ 0 ] << " file1 file2 ..." << endl;
        cout << "Note: If get segmentation fault right away, verify that Tecplot SDK evaluation license" << endl;
        cout << "      file 'sdkeval.lic' is at location specified by environment variable TECSDKHOME." << endl;
        cout << endl;
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
            cout << "\nPreparing to read file: " << inputFileNameAndPath << endl;

            string extension = getExtension( inputFileNameAndPath );
            if( extension.compare("tec") == 0 || extension.compare("plt") == 0 )
            {
	            convertTecplotToVTK( inputFileNameAndPath );
            }
            else
            {
                cout << "Error: This program can only convert tecplot filenames with extensions '.tec' or '.plt'" << endl;
            }
        }

        TecUtilParentLockFinish();

        if( ret == Manager::ManagerStartReturnCode_Ok )
            manager->stop();
        else
            fprintf(stderr, "Tecplot SDK Starup Error: %d\n", ret);
    }
    else
    {
        fprintf(stderr, "Failed to initialize the Tecplot SDK: %s\n", manager->returnCodeString(ret).c_str());
    }

    //free(FinalArgV);
    return ((ret == Manager::ManagerStartReturnCode_Ok) ? 0 : 1);
}

