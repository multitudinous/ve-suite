/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/environment/CameraPlacementEventHandler.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::environment;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler::CameraPlacementEventHandler()
    :
    EventHandler()
{
    //mEventHandlerMap[ "DRUM_ANIMATION_ON_OFF" ] = this;
    mCommandNameToInt[ "DRUM_ANIMATION_ON_OFF" ] =
        DRUM_ANIMATION_ON_OFF;
    //mEventHandlerMap[ "CAMERA_GEOMETRY_ON_OFF" ] = this;
    mCommandNameToInt[ "CAMERA_GEOMETRY_ON_OFF" ] =
        CAMERA_GEOMETRY_ON_OFF;
    //mEventHandlerMap[ "FRUSTUM_GEOMETRY_ON_OFF" ] = this;
    mCommandNameToInt[ "FRUSTUM_GEOMETRY_ON_OFF" ] =
        FRUSTUM_GEOMETRY_ON_OFF;

    //mEventHandlerMap[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] = this;
    mCommandNameToInt[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] =
        DEPTH_OF_FIELD_EFFECT_ON_OFF;
    //mEventHandlerMap[ "PROJECTION_EFFECT_ON_OFF" ] = this;
    mCommandNameToInt[ "PROJECTION_EFFECT_ON_OFF" ] =
        PROJECTION_EFFECT_ON_OFF;
    //mEventHandlerMap[ "PROJECTION_EFFECT_OPACITY" ] = this;
    mCommandNameToInt[ "PROJECTION_EFFECT_OPACITY" ] =
        PROJECTION_EFFECT_OPACITY;

    //mEventHandlerMap[ "CAMERA_WINDOW_ON_OFF" ] = this;
    mCommandNameToInt[ "CAMERA_WINDOW_ON_OFF" ] =
        CAMERA_WINDOW_ON_OFF;
    //mEventHandlerMap[ "CAMERA_WINDOW_RESOLUTION" ] = this;
    mCommandNameToInt[ "CAMERA_WINDOW_RESOLUTION" ] =
        CAMERA_WINDOW_RESOLUTION;

    //mEventHandlerMap[ "DEPTH_HELPER_WINDOW_ON_OFF" ] = this;
    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_ON_OFF" ] =
        DEPTH_HELPER_WINDOW_ON_OFF;
    //mEventHandlerMap[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] = this;
    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] =
        DEPTH_HELPER_WINDOW_RESOLUTION;
    
    //mEventHandlerMap[ "PROJECTION_UPDATE" ] = this;
    mCommandNameToInt[ "PROJECTION_UPDATE" ] =
        PROJECTION_UPDATE;

    //mEventHandlerMap[ "FOCAL_DISTANCE" ] = this;
    mCommandNameToInt[ "FOCAL_DISTANCE" ] =
        FOCAL_DISTANCE;
    //mEventHandlerMap[ "FOCAL_RANGE" ] = this;
    mCommandNameToInt[ "FOCAL_RANGE" ] =
        FOCAL_RANGE;
    //mEventHandlerMap[ "MAX_CIRCLE_OF_CONFUSION" ] = this;
    mCommandNameToInt[ "MAX_CIRCLE_OF_CONFUSION" ] =
        MAX_CIRCLE_OF_CONFUSION;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler::~CameraPlacementEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler::CameraPlacementEventHandler(
    const CameraPlacementEventHandler& ceh )
    :
    ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler& CameraPlacementEventHandler::operator=(
    const CameraPlacementEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementEventHandler::Execute( 
    const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command =
        boost::dynamic_pointer_cast< ves::open::xml::Command >( veXMLObject );
    if( !command )
    {
        return;
    }

    //Set the active camera once the manager is created
    const int commandName =
        mCommandNameToInt.find( command->GetCommandName() )->second;

    switch( commandName )
    {
        case DRUM_ANIMATION_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "drumAnimationOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DrumAnimation( onOff );
        }
        break;

        case CAMERA_GEOMETRY_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "cameraGeometryOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayCamera( onOff );
        }
        break;

        case FRUSTUM_GEOMETRY_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "frustumGeometryOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayViewFrustum( onOff );
        }
        break;

        case DEPTH_OF_FIELD_EFFECT_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "depthOfFieldEffectOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayDepthOfFieldEffect( onOff );
        }
        break;

        case PROJECTION_EFFECT_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "projectionEffectOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayProjectionEffect( onOff );
        }
        break;

        case PROJECTION_EFFECT_OPACITY:
        {
            double value = 0;
            command->GetDataValuePair(
                "projectionEffectOpacity" )->GetData( value );

            //mCameraEntity->SetProjectionEffectOpacity( value );
        }
        break;

        case CAMERA_WINDOW_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "cameraWindowOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayCameraViewQuad( onOff );
        }
        break;

        case CAMERA_WINDOW_RESOLUTION:
        {
            unsigned int value = 0;
            command->GetDataValuePair(
                "cameraWindowResolution" )->GetData( value );

            //mCameraEntity->SetCameraViewQuadResolution( value );
        }
        break;

        case DEPTH_HELPER_WINDOW_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "depthHelperWindowOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DisplayDepthHelperQuad( onOff );
        }
        break;

        case DEPTH_HELPER_WINDOW_RESOLUTION:
        {
            unsigned int value = 0;
            command->GetDataValuePair(
                "depthHelperWindowResolution" )->GetData( value );

            //mCameraEntity->SetDepthHelperQuadResolution( value );
        }
        break;

        case PROJECTION_UPDATE:
        {
            double projectionData[ 4 ] = { 0, 0, 0, 0 };
            command->GetDataValuePair(
                "projectionFieldOfView" )->GetData( projectionData[ 0 ] );
            command->GetDataValuePair(
                "projectionAspectRatio" )->GetData( projectionData[ 1 ] );
            command->GetDataValuePair(
                "projectionNearPlane" )->GetData( projectionData[ 2 ] );
            command->GetDataValuePair(
                "projectionFarPlane" )->GetData( projectionData[ 3 ] );

            //mCameraEntity->setProjectionMatrixAsPerspective(
            //    projectionData[ 0 ], projectionData[ 1 ],
            //    projectionData[ 2 ], projectionData[ 3 ] );

            //mCameraEntity->Update();
        }
        break;

        case FOCAL_DISTANCE:
        {
            double value = 0;
            command->GetDataValuePair(
                "focalDistance" )->GetData( value );

            //mCameraEntity->SetFocalDistance( value );
        }
        break;

        case FOCAL_RANGE:
        {
            double value = 0;
            command->GetDataValuePair(
                "focalRange" )->GetData( value );

            //mCameraEntity->SetFocalRange( value );
        }
        break;

        case MAX_CIRCLE_OF_CONFUSION:
        {
            double value = 0;
            command->GetDataValuePair(
                "maxCircleOfConfusion" )->GetData( value );

            //mCameraEntity->SetMaxCircleOfConfusion( value );
        }
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
