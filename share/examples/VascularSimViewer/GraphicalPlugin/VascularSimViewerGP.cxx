// --- Header Include --- //
#include "VascularSimViewerGP.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- C++ Include --- //
#include <iostream>

// --- This Plugin Includes --- //
#include "FileIOCommandHandler.h"
#include "VizCommandHandler.h"

using namespace vascularsimviewer;

////////////////////////////////////////////////////////////////////////////////
VascularSimViewerGP::VascularSimViewerGP()
    :
    PluginBase()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "VascularSimViewerUI";

    mEventHandlerMap[ "VASCULAR_SIM_VIEWER_COMMAND" ] = this;  //map variable from PluginBase.h  This is needed so that conductor commands with the name VASCULAR_SIM_VIEWER_COMMAND are sent here 
}
////////////////////////////////////////////////////////////////////////////////
VascularSimViewerGP::~VascularSimViewerGP()
{
    delete _IOCommandHandler;
    delete _VizCommandHandler;
}
////////////////////////////////////////////////////////////////////////////////
void VascularSimViewerGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    _IOCommandHandler = new FileIOCommandHandler();
    _VizCommandHandler = new VizCommandHandler( mDCS.get() );
    
    _IOCommandHandler->SetVizCommandHandler( _VizCommandHandler );
}
////////////////////////////////////////////////////////////////////////////////
void VascularSimViewerGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VascularSimViewerGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "Set Command Begin" << std::endl;
    if( !command )
    {
        return;
    }
    const std::string commandName = command->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = command->GetDataValuePair( 0 );
    
    if( dvp->GetDataName() == "FILE_IO_COMMAND" )
    {
        _IOCommandHandler->ProcessCurrentCommand( command );
        
    }
    else if( dvp->GetDataName() == "VIZ_COMMAND" )
    {
        std::cout << "Viz Command" << std::endl;
        _VizCommandHandler->ProcessCurrentCommand( command );        
    }
    else
    {
        std::cout << "OOPS" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
