#include "NetworkTwoGP.h"

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/Command.h>

///////////////////////////////////////////////////////////////////////////////
NetworkTwoGP::NetworkTwoGP() : PluginBase()
{

}
///////////////////////////////////////////////////////////////////////////////
NetworkTwoGP::~NetworkTwoGP()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoGP
::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoGP::PreFrameUpdate()
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoGP::SetCurrentCommand( ves::open::xml::CommandPtr )
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwoGP::CreateCustomVizFeature( int )
{

}
