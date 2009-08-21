// Timothy Gundere July 9, 2009

// --- Header Includes --- //
#include "FileIOCommandHandler.h"
#include "VizCommandHandler.h"

// --- Plugin Includes --- //
#include "vtkUGDataSet.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- C++ Includes --- //
#include <iostream> //I include this for debugging


using namespace vascularsimviewer;

// Constructor ////////////////////////////////////
FileIOCommandHandler::FileIOCommandHandler()
    :
    _activeDataSet( NULL ),
    mVizCommandHandler( NULL )
{
    //std::cout << "DCS point is: " << mPluginDCS << std::endl;
}

// Destructor ////////////////////////////////////
FileIOCommandHandler::~FileIOCommandHandler()
{
    //TODO - Loop to delete vector of vtkUGDataSets - This is a memory leak;
}

////////////////////////////////////////////
void FileIOCommandHandler::SetVizCommandHandler( VizCommandHandler* inVizCommandHandler )
{
    mVizCommandHandler = inVizCommandHandler;
}

// Process Current Command
void FileIOCommandHandler::ProcessCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
    
    const std::string commandName = command->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = command->GetDataValuePair( 0 );
    std::vector< std::string > _commandVector;
    dvp->GetData( _commandVector );
    
    std::vector< std::string > _enabledArrays;
    
    if( _commandVector[0] == "LOAD_VTU_FILE" )
    {
        std::cout << "Loading .vtu file" << std::endl;
        
        std::string _filename( _commandVector[1] );
        
        // Loop to get active arrays
        int i;
        for( i=2; i<_commandVector.size(); i++ )
        {
            _enabledArrays.push_back( std::string ( _commandVector[i] ) );
        }
        
        _DataSets.push_back( new vtkUGDataSet( _filename, _enabledArrays ) );
        
        //mVizCommandHandler->CreateVisualization( _DataSets.back(), "SHOW_SURFACE" );
        mVizCommandHandler->CreateVisualization( _DataSets.back(), "SHOW_WSS_MAG" );
    }
    else if( _commandVector[0] == "UPDATE_VTU_FILE" )
    {
        std::cout << "Updating .vtu file" << std::endl;
        
        std::string _filename( _commandVector[1] );
        
        // Loop to get active arrays
        int i;
        for( i=2; i<_commandVector.size(); i++ )
        {
            _enabledArrays.push_back( std::string( _commandVector[i] ) );
        }
        
        SetActiveDataSetByFilename( _filename );
        _activeDataSet->UpdateArrays( _enabledArrays );
    }
    else
    {
        std::cout << "OOPS" << std::endl;
    }
}

// Scan .vtu file ////////////////////////////////
void FileIOCommandHandler::LoadVTUFile( const std::string& filename, std::vector< std::string > _arrays )
{
}
    
/////////////////////////////////////////////////////
void FileIOCommandHandler::SetActiveDataSetByFilename( const std::string& filename )
{
    int i;
    
    std::string _tmpFilename;
    
    // Loop thru datasets until matching filename is found
    for ( i=0; i<_DataSets.size() ; i++ )
    {
        _tmpFilename = _DataSets[i]->GetFilename();
        
        if( !std::strcmp( _tmpFilename.c_str(),filename.c_str() ) )
        {
            _activeDataSet = _DataSets[i];
        }
        _tmpFilename.clear();
    }
}

////////////////////////////////////////////////////////
vtkUGDataSet* FileIOCommandHandler::GetActiveDataSet()
{
    return _activeDataSet;
}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


