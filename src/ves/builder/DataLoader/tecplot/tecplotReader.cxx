#include "TecIntegrationManager.h"
#include "Manager.h"
#include "ApplicationEventMonitor.h"
#include "StringList.h"
#include <iostream>

using namespace tecplot::sdk::integration;
using namespace tecplot::toolbox;
using namespace std;

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

    VarName_t Name;
    for( int i = 1; i < nvars+1; i++ ) // variable numbers are 1-based
    {
        TecUtilVarGetName( i, &Name );
        cout << "The name of Variable " << i << " is \"" << Name << "\"" << endl;
        TecUtilStringDealloc( &Name );
    }

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
        // If the frame mode is XY the handles must be passed in as NULL. Otherwise, passing NULL indicates the value is not desired.
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
            cout << "The number of data points is " << IMax << endl;

            numElements = JMax;
            cout << "The number of elements is " << numElements << endl;

            if( zoneType == ZoneType_FEPolygon || zoneType == ZoneType_FEPolyhedron )
            {
                // face-based FE-data
                cout << "The number of faces per cell is " << KMax << endl;
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

        cout << "The number of nodes per element is " << numNodesPerElement << endl;

        NodeMap_pa nm = TecUtilDataNodeGetReadableRef( currentZone );
        if( nm )
        {
            for( LgIndex_t elemNum = 1; elemNum < numElements+1; elemNum++ ) // element numbers are 1-based
            {
                cout << "For element " << elemNum << ", nodes =";

                // Node information (connectivity)
                // NOTE - You could use the "RawPtr" functions if speed is a critical issue
                NodeMap_t node[ numNodesPerElement ];
                for( int i = 1; i < numNodesPerElement+1; i++ ) // node numbers are 1-based
                {
                    node[ i-1 ] = TecUtilDataNodeGetByRef( nm, elemNum, i );
                    cout << " " << node[ i-1 ];
                }
                cout << endl;
            }
        }
        else
        {
            cerr << "Error: Unable to get node map" << endl;
        }

        // Read some variable data from the current zone...
        EntIndex_t VarNum = 1;

        FieldData_pa FieldData = TecUtilDataValueGetReadableNativeRef( currentZone, VarNum );
        if( FieldData )
        {
            LgIndex_t NumValues = TecUtilDataValueGetCountByRef( FieldData );
            double Var[ NumValues ];

            cout << "Writing Variable " << VarNum << ":" << endl;
            for( int i = 0; i < NumValues; i++ )
            {
                Var[ i ] = TecUtilDataValueGetByRef( FieldData, i+1 ); //GetByRef function is 1-based
                cout << "Var[" << i << "] = " << Var[ i ] << "\n";
            }
        }
        else
        {
            cerr << "Error: Unable to read variable data" << endl;
        }

    } // for each zone
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

