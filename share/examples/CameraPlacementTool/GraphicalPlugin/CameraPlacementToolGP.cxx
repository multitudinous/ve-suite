// --- My Includes --- //
#include "CameraPlacementToolGP.h"
#include "CameraPlacementToolScene.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::CameraPlacementToolGP()
:
PluginBase(),
mScene()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "CameraPlacementToolUI";

    mEventHandlerMap[ "SHADER_EFFECTS_UPDATE" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::~CameraPlacementToolGP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::InitializeNode(
    ves::xplorer::scenegraph::DCS* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    mScene = cpt::CameraPlacementToolScenePtr(
        new cpt::CameraPlacementToolScene( mDCS.get() ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::UpdateParams()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }

    //double data[ 3 ] = { 0 };

    //Set current shader effect
    if( command->GetCommandName() == "SHADER_EFFECTS_UPDATE" )
    {
        //unsigned int temp = 0;
        //command->GetDataValuePair( "shaderEffects" )->GetData( temp );

        //if( temp == 0 )
        //{
            //m_scene->DefaultVisuals();
        //}
    }
    else if( command->GetCommandName() == "AMBIENT_UPDATE" )
    {
        //command->GetDataValuePair( "arColor" )->GetData( data[ 0 ] );
        //command->GetDataValuePair( "agColor" )->GetData( data[ 1 ] );
        //command->GetDataValuePair( "abColor" )->GetData( data[ 2 ] );

        //m_scene->GetLight()->setAmbient( osg::Vec4( data[ 0 ], data[ 1 ], data[ 2 ], 1.0f ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
