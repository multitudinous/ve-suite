#include "NetworkThreeGP.h"

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/Command.h>

///////////////////////////////////////////////////////////////////////////////
NetworkThreeGP::NetworkThreeGP() : PluginBase()
{

}
///////////////////////////////////////////////////////////////////////////////
NetworkThreeGP::~NetworkThreeGP()
{
	;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeGP
::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
	PluginBase::InitializeNode( veworldDCS );
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeGP::PreFrameUpdate()
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{

}
///////////////////////////////////////////////////////////////////////////////
void NetworkThreeGP::CreateCustomVizFeature( int input )
{

}