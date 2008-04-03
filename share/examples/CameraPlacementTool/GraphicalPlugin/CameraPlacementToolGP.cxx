/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "CameraPlacementToolGP.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::CameraPlacementToolGP()
:
PluginBase(),
mCameraEntity( 0 )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "CameraPlacementToolUI";

    mEventHandlerMap[ "TOGGLE_CAMERA_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_FRUSTUM_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_PROJECTION_UPDATE" ] = this;
    mEventHandlerMap[ "PROJECTION_UPDATE" ] = this;
    mEventHandlerMap[ "VIEW_PERSPECTIVE_UPDATE" ] = this;
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

    //Initialize the CameraEntity
    mCameraEntity = new cpt::CameraEntity( mSceneManager );

    double cameraPosition[ 3 ] = { 0, -5.0, 0 };
    mCameraEntity->GetDCS()->SetTranslationArray( cameraPosition );
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
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleCamera" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayCamera( onOff );
    }
    else if( command->GetCommandName() == "TOGGLE_FRUSTUM_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleFrustum" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayViewFrustum( onOff );
    }
    else if( command->GetCommandName() == "TOGGLE_PROJECTION_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleProjection" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayProjectionEffect( onOff );
    }
    else if( command->GetCommandName() == "PROJECTION_UPDATE" )
    {
        double projectionData[ 4 ] = { 0, 0, 0, 0 };
        command->GetDataValuePair(
            "projectionFoVZ" )->GetData( projectionData[ 0 ] );
        command->GetDataValuePair(
            "projectionAspectRatio" )->GetData( projectionData[ 1 ] );
        command->GetDataValuePair(
            "projectionNearPlane" )->GetData( projectionData[ 2 ] );
        command->GetDataValuePair(
            "projectionFarPlane" )->GetData( projectionData[ 3 ] );

        mCameraEntity->setProjectionMatrixAsPerspective(
            projectionData[ 0 ], projectionData[ 1 ],
            projectionData[ 2 ], projectionData[ 3 ] );

        mCameraEntity->Update();
    }
    else if( command->GetCommandName() == "VIEW_PERSPECTIVE_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "viewPerspective" )->GetData( selection );

        bool onOff = ( selection == 0 );
        mCameraEntity->DisplayScreenAlignedQuad( onOff );
    }
}
////////////////////////////////////////////////////////////////////////////////
