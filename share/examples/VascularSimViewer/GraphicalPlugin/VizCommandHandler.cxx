// Timothy Gundere July 9, 2009

// --- Header Includes --- //
#include "VizCommandHandler.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- This Plugin Includes --- //
#include "PolyVisualizationBase.h"
#include "ShowSurfaceFilter.h"
#include "wssMagSurfFilter.h"
#include "vtkUGDataSet.h"

// --- C++ Includes --- //
#include <iostream> //I include this for debugging
#include <string>



using namespace vascularsimviewer;

// Constructor ////////////////////////////////////
VizCommandHandler::VizCommandHandler( ves::xplorer::scenegraph::DCS* parentDCS )
    :
    mPluginDCS( parentDCS )
{
    ;
}

// Destructor ////////////////////////////////////
VizCommandHandler::~VizCommandHandler()
{
    //Need loop to delete all viz objects
    ;
}

// Process Current Command
void VizCommandHandler::ProcessCurrentCommand( ves::open::xml::CommandPtr command )
{
    ;
}

// Create Visualization
void VizCommandHandler::CreateVisualization( vtkUGDataSet* inDataSet, const std::string& inCommand )
{
    if( inCommand == "SHOW_SURFACE" )
    {
        mVisualizations.push_back( new ShowSurfaceFilter( inDataSet, mPluginDCS ) );
        inDataSet->AddVisualization( mVisualizations.back() );
    }
    else if( inCommand == "SHOW_WSS_MAG" )
    {
        mVisualizations.push_back( new wssMagSurfFilter( inDataSet, mPluginDCS ) );
        inDataSet->AddVisualization( mVisualizations.back() );
    }
    else
    {
        std::cout << "Not A Valid Visualization Command, YET" << std::endl;
    }
}
