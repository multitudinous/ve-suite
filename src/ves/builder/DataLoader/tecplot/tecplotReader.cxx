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

#include <vtkUnstructuredGrid.h>    // -lvtkFiltering
#include <vtkPoints.h>              // -lvtkCommon
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace std;

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
            cout << "Writing Variable " << varName << ":" << endl;
            for( int i = 0; i < NumValues; i++ )
            {
                array[ i ] = TecUtilDataValueGetByRef( FieldData, i+1 ); //GetByRef function is 1-based
                cout << varName << "[" << i+1 << "] = " << array[ i ] << endl;
            }
        }
        else
        {
            cerr << "Error: Unable to read " << varName << " variable data" << endl;
            return array;
        }
    }
    return array;
}

void convertTecplotToVTK( char * fName )
{
    std::string inFName(fName);
    cout << "\nPreparing to read file: " << inFName << endl;
    StringList fileName(inFName.c_str(), NULL);

    Boolean_t IsOk =
    TecUtilReadDataSet(ReadDataOption_NewData,    // NewData = Remove data set fron current frame before loading new data set
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
                       1, 1, 1);                  // Set to 1 to load every data point in the I-direction, J-direction, & K-direction. 

    char *dataset_title = NULL;
    EntIndex_t nzones, nvars;
    TecUtilDataSetGetInfo(&dataset_title, &nzones, &nvars);
    cout << "The dataset_title is \"" << dataset_title << "\"" << endl;
    cout << "Number of zones is " << nzones << " and number of variables is " << nvars << endl;
    TecUtilStringDealloc(&dataset_title);

    VarName_t * Name = new VarName_t [ nvars ];
    int xIndex = 0;
    int yIndex = 0;
    int zIndex = 0;
    int numNodalParameters = 0;

    for( int i = 0; i < nvars; i++ )
    {
        TecUtilVarGetName( i+1, &Name[ i ] ); // variable numbers are 1-based
        cout << "The name of Variable " << i+1 << " is \"" << Name[ i ] << "\"" << endl;
        if( strcmp( Name[ i ], "X" ) == 0 )
        {
            xIndex = i+1;
        }
        else if( strcmp( Name[ i ], "Y" ) == 0 )
        {
            yIndex = i+1;
        }
        else if( strcmp( Name[ i ], "Z" ) == 0 )
        {
            zIndex = i+1;
        }
        else
        {
            // count number of non-coordinate nodal data parameters
            numNodalParameters++;
        }
    }
    cout << "xIndex is " << xIndex << ", yIndex is " << yIndex << ", zIndex is " << zIndex << endl;
    cout << "numNodalParameters is " << numNodalParameters << endl;

    vtkUnstructuredGrid *ugrid = vtkUnstructuredGrid::New();

    for( EntIndex_t currentZone = 1; currentZone < nzones+1; currentZone++ ) // zone numbers are 1-based
    {
        cout << "For Zone " << currentZone << ", ";
        ZoneType_e zoneType = TecUtilZoneGetType( currentZone );
        switch( zoneType )
        {
            case ZoneType_Ordered:
                cout << "ZoneType_Ordered" << endl;
                break;
            case ZoneType_FETriangle:
                cout << "ZoneType_FETriangle" << endl;
                break;
            case ZoneType_FEQuad:
                cout << "ZoneType_FEQuad" << endl;
                break;
            case ZoneType_FETetra:
                cout << "ZoneType_FETetra" << endl;
                break;
            case ZoneType_FEBrick:
                cout << "ZoneType_FEBrick" << endl;
                break;
            case ZoneType_FELineSeg:
                cout << "ZoneType_FELineSeg" << endl;
                break;
            case ZoneType_FEPolygon:
                cout << "ZoneType_FEPolygon" << endl;
                break;
            case ZoneType_FEPolyhedron:
                cout << "ZoneType_FEPolyhedron" << endl;
                break;
            case END_ZoneType_e:
                cout << "END_ZoneType_e" << endl;
                break;
            case ZoneType_Invalid:
                cout << "ZoneType_Invalid" << endl;
                break;
            default:
                cout << "ZoneType not known. Not supposed to get here." << endl;
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

        int numDataPoints = 0;
        int numNodesPerElement = 0;
        int numFacesPerCell = 0;
        LgIndex_t numElements = 0;

        if( zoneType == ZoneType_Ordered )
        {
            cout << "The I-dimension for ordered data is " << IMax << endl;
            cout << "The J-dimension for ordered data is " << JMax << endl;
            cout << "The K-dimension for ordered data is " << KMax << endl;
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
            cout << "The number of data points is " << numDataPoints << endl;

            numElements = JMax;
            cout << "The number of elements is " << numElements << endl;

            if( zoneType == ZoneType_FEPolygon || zoneType == ZoneType_FEPolyhedron )
            {
                // face-based FE-data
                numFacesPerCell = KMax;
                cout << "The number of faces per cell is " << numFacesPerCell << endl;
            }
            else
            {
                // cell-based FE-data
                numNodesPerElement = KMax;
                cout << "The number of nodes per cell is " << numNodesPerElement << endl;
            }
        }
        else
        {
            cout << "ZoneType not known. Not supposed to get here." << endl;
        }

        if( numNodesPerElement == 0 )
            cout << "The number of nodes per element is " << numNodesPerElement << endl;

        NodeMap_pa nm = TecUtilDataNodeGetReadableRef( currentZone );
        if( nm )
        {
            for( LgIndex_t elemNum = 1; elemNum < numElements+1; elemNum++ ) // element numbers are 1-based
            {
                cout << "For element " << elemNum << ", nodes =";

                // Node information (connectivity)
                // NOTE - You could use the "RawPtr" functions if speed is a critical issue
                NodeMap_t nodeArray[ numNodesPerElement ];
                for( int i = 0; i < numNodesPerElement; i++ ) 
                {
                    nodeArray[ i ] = TecUtilDataNodeGetByRef( nm, elemNum, i+1 ); // node numbers are 1-based
                    cout << " " << nodeArray[ i ];
                }
                cout << endl;

                if( zoneType == ZoneType_FEBrick )
                {
                    ugrid->InsertNextCell( VTK_HEXAHEDRON, numNodesPerElement, nodeArray );
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
        double * y = readVariable( currentZone, yIndex, "Y" );
        double * z = readVariable( currentZone, zIndex, "Z" );

        vtkPoints *vertex = vtkPoints::New();
        for( int i = 0; i < numDataPoints; i++ )
        {
            vertex->InsertPoint( i+1 , x[ i ], y[ i ], z[ i ] );
        }

        ugrid->SetPoints( vertex );
        vertex->Delete();

        // set up arrays to store scalar nodal data over entire mesh...
        vtkFloatArray ** parameterData = new vtkFloatArray * [ numNodalParameters ];
/*
        for( int i = 0; i < numNodalParameters; i++ )
        {
            parameterData[ i ] = vtkFloatArray::New();
        }
*/
        int ii = 0;
        double * tempArray;
        for( int i = 0; i < nvars; i++ )
        {
            if( strcmp( Name[ i ], "X" ) == 0 ||
                strcmp( Name[ i ], "Y" ) == 0 ||
                strcmp( Name[ i ], "Z" ) == 0 )
            {
                // nodal coordinates already processed
            }
            else
            {
                // Read the parameter data from the current zone...
                tempArray = readVariable( currentZone, i, Name[ i ] );
                parameterData[ ii ] = vtkFloatArray::New();
                parameterData[ ii ]->SetNumberOfTuples( numDataPoints );
                //dirStringStream << "displacement mag " << currentDataSetSolution << " " << displacementUnits;
                parameterData[ ii ]->SetName( Name[ i ] ); //dirStringStream.str().c_str() );
                parameterData[ ii ]->SetNumberOfComponents( 1 );
                //dirStringStream.str( "" );
                for( int j = 0; j < numDataPoints; j++ )
                {
                    parameterData[ ii ]->SetTuple1( j, tempArray[ j ] );
                }
                delete [] tempArray;
            }
        }

        for( int i = 0; i < numNodalParameters; i++ )
        {
            ugrid->GetPointData()->AddArray( parameterData[ i ] );
            parameterData[ i ]->Delete();
        }

        delete [] parameterData;

    } // for each zone

    for( int i = 0; i < nvars; i++ )
    {
        TecUtilStringDealloc( &Name[ i ] );
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
	        convertTecplotToVTK( argv[ i ] );
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

