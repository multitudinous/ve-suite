//--- Header Include ---//
#include "DataSet.h"

// --- vtk Includes --- //
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkDataArraySelection.h>

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- This Plugin Includes --- //
#include "DataSetArray.h"
#include "DataSetArraySelectionDialog.h"
#include "VascularSimViewerUIDialog.h"
#include "LoadDataGUI.h"
#include "vesCustomXMLUGReader.h"

// --- C++ Includes --- //
#include <iostream>

using namespace vascularsimviewer;

// Constructor
DataSet::DataSet( const std::string &inFilename, VascularSimViewerUIDialog* UIDialog, ves::conductor::util::CORBAServiceList* service )
    :   
    mFilename( inFilename ),
    VascSimUIDialog( UIDialog ),
    mServiceList( service )
{
    // TODO - Check to see if input is vtu or vtk
    
    ScanVTUFile();
    
    //When new DataSet is created, also create an associated Array Selection Dialog
    DataSetDialog = new DataSetArraySelectionDialog( wxID_ANY, wxT("Array Selection Dialog"), mPtDataArrays.size(), this );
        
    ShowDialog();   
}

// Desctructor
DataSet::~DataSet()
{
    DataSetDialog->Destroy();
    
    //Not sure if this teh correct way of doing this
    std::vector< DataSetArray* >::iterator iter;
    for( iter=mPtDataArrays.begin(); iter != mPtDataArrays.end(); iter++ )
    {
        delete *iter;
    }
    mPtDataArrays.clear();
    
    std::cout << "Data Set" << mFilename << "Deleted" << std::endl;
}

////////////////////////////
const std::string& DataSet::GetFileName()
{
    return mFilename;
}

// Scan .vtu file // -- Should only get called in constructor
void DataSet::ScanVTUFile()
{	
    vesCustomXMLUGReader* _reader = new vesCustomXMLUGReader();
	_reader->SetFileName( mFilename.c_str() );
	_reader->Update();
    
    //New Reader Test    
    std::cout << "Number of Point Arrays is: " << _reader->GetNumberofPointArrays() << std::endl;
    
    
    
	int i; //for loop counter
	
	// Point Array Scanning ///////////////////////////////
	int _numPtArr = _reader->GetNumberOfPointArrays();
	
    for( i=0; i<_numPtArr; i++ )
    {
        mPtDataArrays.push_back( new DataSetArray( this ) );
        mPtDataArrays.back()->SetArrayName( std::string( _reader->GetPointArrayName(i) ) );
    }
	
    // Cell Array Scanning // --None of the DataSets should have Cell Data
/*    int _numCellArr = _reader->GetNumberOfCellArrays();
    std::cout << "No. Cell Arrays is " << _numCellArr << std::endl;
	
	if ( _numCellArr != 0 )
    {
	std::vector< std::string > _cellArrayNames( _numCellArr - 1 );

	for ( i=0; i<_numCellArr; i++ )
        {	
            _cellArrayNames[i] = std::string(_reader->GetCellArrayName(i));
            std::cout << "Cell Array Name: " << _reader->GetCellArrayName(i) << std::endl;
        }
    } */
    //delete _reader; 
}

////////////////////////////////////////
int DataSet::GetNumArrays()
{
    return mPtDataArrays.size();
}

///////////////////////////////////////
DataSetArray* DataSet::GetPtArray( int arrayIndex )
{
    return mPtDataArrays[arrayIndex];
}

///////////////////////////////////////
void DataSet::ShowDialog()
{
    DataSetDialog->ShowModal();  //ShowModal means nothing else can be clicked until this dialog closes
}

/////////////////////////////////////////
LoadDataGUI* DataSet::GetLoadDataGui()
{
    return VascSimUIDialog->GetLoadDataGuiPtr();
}

////////////////////////////////////////
void DataSet::LoadDataInXplorer()
{
    ves::open::xml::DataValuePairSharedPtr vascularSimViewerDVP( new ves::open::xml::DataValuePair() );  //Create new DataValuePair (DVP)
	
    std::vector< std::string > _commandVector;  //vector will be formatted as [ COMMAND, Filename, _enabled datasets... ]
    
    if( GetLoadDataGui()->FileIsAlreadyLoaded( mFilename ) )
    {
        _commandVector.push_back( "UPDATE_VTU_FILE" );
        _commandVector.push_back( mFilename );
    }
    else
    {
        _commandVector.push_back( "LOAD_VTU_FILE" );
        _commandVector.push_back( mFilename );
    }
     
    int i;
    for( i=0; i<mPtDataArrays.size(); i++ )
    {
        if( mPtDataArrays[i]->GetArrayStatus() )
        {
            _commandVector.push_back( mPtDataArrays[i]->GetArrayName() );
        }
    }

    // Most of this block was copied from the WarrantyTool - See other plugins for how to send commands to Xplorer
    vascularSimViewerDVP->SetData( "FILE_IO_COMMAND", _commandVector );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); //Taken from WarrantyToolUIDialog.cxx [227-231]
    command->AddDataValuePair( vascularSimViewerDVP );
    std::string mCommandName = "VASCULAR_SIM_VIEWER_COMMAND";  //Used to map this command to graphical plugin
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
