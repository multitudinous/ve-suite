// --- My Includes --- //
#include "GraphicalPlugin.h"
#include "Scene.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
GraphicalPlugin::GraphicalPlugin()
:
cfdVEBaseClass(),
m_scene( 0 )
{
    //Needs to match inherited UIPluginBase class name
    m_objectName = "UserInterfacePlugin";
}
////////////////////////////////////////////////////////////////////////////////
GraphicalPlugin::~GraphicalPlugin()
{
    if( m_scene )
    {
        delete m_scene;   
    }
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPlugin::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    cfdVEBaseClass::InitializeNode( veworldDCS );

    m_scene = new cpt::Scene( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPlugin::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPlugin::UpdateParams()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GraphicalPlugin::SetCurrentCommand( ves::open::xml::Command* command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////

