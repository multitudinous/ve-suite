#include "NetworkOneGP.h"

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/Command.h>

///////////////////////////////////////////////////////////////////////////////
NetworkOneGP::NetworkOneGP() : PluginBase()
{

}
///////////////////////////////////////////////////////////////////////////////
NetworkOneGP::~NetworkOneGP()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP
::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::PreFrameUpdate()
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::SetCurrentCommand( ves::open::xml::CommandPtr )
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::CreateCustomVizFeature( int )
{

}
