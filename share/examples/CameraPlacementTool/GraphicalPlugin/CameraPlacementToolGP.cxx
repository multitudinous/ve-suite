// --- My Includes --- //
#include "CameraPlacementToolGP.h"
#include "CameraPlacementToolScene.h"
#include "CameraEntity.h"

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

    mEventHandlerMap[ "TOGGLE_CAMERA_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_FRUSTUM_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_PROJECTION_UPDATE" ] = this;
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

    if( command->GetCommandName() == "TOGGLE_CAMERA_UPDATE" )
    {
        unsigned int data = 0;
        command->GetDataValuePair( "toggleCamera" )->GetData( data );

        bool onOff = ( data != 0 );
        mScene->GetActiveCameraEntity()->DrawCameraGeometry( onOff );
    }
    else if( command->GetCommandName() == "TOGGLE_FRUSTUM_UPDATE" )
    {
        unsigned int data = 0;
        command->GetDataValuePair( "toggleFrustum" )->GetData( data );

        bool onOff = ( data != 0 );
        mScene->GetActiveCameraEntity()->DrawViewFrustum( onOff );
    }
    else if( command->GetCommandName() == "TOGGLE_PROJECTION_UPDATE" )
    {
        unsigned int data = 0;
        command->GetDataValuePair( "toggleProjection" )->GetData( data );

        bool onOff = ( data != 0 );
        //mScene->GetActiveCameraEntity()->GetDCS()->setNodeMask( onOff );
    }
}
////////////////////////////////////////////////////////////////////////////////
