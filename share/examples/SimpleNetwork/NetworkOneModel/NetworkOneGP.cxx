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
::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
	PluginBase::InitializeNode( veworldDCS );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::PreFrameUpdate()
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::SetCurrentCommand( ves::open::xml::Command* command )
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkOneGP::CreateCustomVizFeature( int input )
{

}