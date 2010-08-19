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

// --- VES Includes --- //
#include <ves/xplorer/event/environment/CameraPlacementEventHandler.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/highlight/HighlightManager.h>
#include <ves/xplorer/scenegraph/highlight/CircleHighlight.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

// --- VRJ Includes --- //
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::environment;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler::CameraPlacementEventHandler()
    :
    EventHandler()
{
    mCommandNameToInt[ "ADD_CAMERA_OBJECT" ] =
        ADD_CAMERA_OBJECT;
    mCommandNameToInt[ "SELECT_CAMERA_OBJECT" ] =
        SELECT_CAMERA_OBJECT;
    mCommandNameToInt[ "DELETE_CAMERA_OBJECT" ] =
        DELETE_CAMERA_OBJECT;
    mCommandNameToInt[ "REMOVE_ALL_CAMERA_OBJECTS" ] =
        REMOVE_ALL_CAMERA_OBJECTS;

    mCommandNameToInt[ "SAVE_CAMERA_IMAGE" ] =
        SAVE_CAMERA_IMAGE;
    mCommandNameToInt[ "SAVE_ALL_CAMERA_IMAGES" ] =
        SAVE_ALL_CAMERA_IMAGES;

    mCommandNameToInt[ "TOGGLE_HIGHLIGHT_TOOL" ] =
        TOGGLE_HIGHLIGHT_TOOL;
    mCommandNameToInt[ "SELECT_MARKER_OBJECT" ] =
        SELECT_MARKER_OBJECT;
    mCommandNameToInt[ "DELETE_MARKER_OBJECT" ] =
        DELETE_MARKER_OBJECT;
    mCommandNameToInt[ "REMOVE_ALL_MARKER_OBJECTS" ] =
        REMOVE_ALL_MARKER_OBJECTS;

    mCommandNameToInt[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] =
        DEPTH_OF_FIELD_EFFECT_ON_OFF;
    mCommandNameToInt[ "PROJECTION_EFFECT_ON_OFF" ] =
        PROJECTION_EFFECT_ON_OFF;
    mCommandNameToInt[ "PROJECTION_EFFECT_OPACITY" ] =
        PROJECTION_EFFECT_OPACITY;

    mCommandNameToInt[ "CAMERA_WINDOW_ON_OFF" ] =
        CAMERA_WINDOW_ON_OFF;
    mCommandNameToInt[ "CAMERA_WINDOW_RESOLUTION" ] =
        CAMERA_WINDOW_RESOLUTION;

    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_ON_OFF" ] =
        DEPTH_HELPER_WINDOW_ON_OFF;
    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] =
        DEPTH_HELPER_WINDOW_RESOLUTION;

    mCommandNameToInt[ "CAMERA_GEOMETRY_ON_OFF" ] =
        CAMERA_GEOMETRY_ON_OFF;
    mCommandNameToInt[ "FRUSTUM_GEOMETRY_ON_OFF" ] =
        FRUSTUM_GEOMETRY_ON_OFF;

    mCommandNameToInt[ "PROJECTION_UPDATE" ] =
        PROJECTION_UPDATE;

    mCommandNameToInt[ "FOCAL_DISTANCE" ] =
        FOCAL_DISTANCE;
    mCommandNameToInt[ "FOCAL_RANGE" ] =
        FOCAL_RANGE;
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

    //Set the active cameraObject once the manager is created
     int commandName =
         mCommandNameToInt.find( command->GetCommandName() )->second;

    DeviceHandler& deviceHandler = *(DeviceHandler::instance());
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();
    scenegraph::highlight::HighlightManager& highlightManager =
        sceneManager.GetHighlightManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        sceneManager.GetManipulatorManager().GetSceneManipulator();
    scenegraph::camera::CameraManager& cameraManager =
        sceneManager.GetCameraManager();

    switch( commandName )
    {
    case ADD_CAMERA_OBJECT:
    {
        std::string name;
        command->GetDataValuePair( "addCameraObject" )->GetData( name );

        cameraManager.addChild( name );

        //break;
    }
    case SELECT_CAMERA_OBJECT:
    {
        unsigned int selection;
        command->GetDataValuePair(
            "selectCameraObject" )->GetData( selection );

        deviceHandler.UnselectObjects();

        scenegraph::camera::CameraObject* newCameraObject =
            cameraManager.ConvertNodeToCameraObject(
                cameraManager.getChild( selection ) );

        cameraManager.SetActiveCameraObject( newCameraObject );

        //Right now we are saying you must have a DCS
        scenegraph::DCS& selectedDCS = newCameraObject->GetDCS();
        gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();

        //Set the connection between the scene manipulator and the selected dcs
        sceneManipulator->Connect( &selectedDCS );

        //If dcs is from a camera object, we want to rotate about local zero point
        osg::Vec3d center( 0.0, 0.0, 0.0 );
        center = center * osg::Matrixd( selectedMatrix.mData );
        sceneManipulator->SetPosition( center );

        //We need to transform center point into camera space
        //In the future the center point will be in world coordinates
        center = center * osg::Matrixd( sceneManager.GetWorldDCS()->GetMat().mData );
        gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
        deviceHandler.SetCenterPoint( &tempCenter );

        //Set the selected DCS
        deviceHandler.SetSelectedDCS( &selectedDCS );

        //Need to do this for multi-pass techniques
        if( sceneManager.IsRTTOn() )
        {
            selectedDCS.SetTechnique( "Glow" );
        }
        else
        {
            selectedDCS.SetTechnique( "Select" );
        }

        //Hand the node we are interested in off to the animation engine
        NavigationAnimationEngine& nae =
            *(NavigationAnimationEngine::instance());
        nae.SetDCS( sceneManager.GetWorldDCS() );

        //
        osg::Vec3d eye, up, ctr;
        osg::Matrixd viewMatrix( selectedMatrix.mData );
        viewMatrix.getLookAt( eye, ctr, up );
        osg::Vec3d viewVector = ctr - eye;
        viewVector.normalize();
        viewVector *= 10.0;

        //Hand our created end points off to the animation engine
        selectedMatrix = gmtl::invert( selectedMatrix );
        gmtl::Vec3d navToPoint =
            gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
        navToPoint += gmtl::Vec3d( viewVector.x(), -viewVector.z(), viewVector.y() );
        gmtl::Quatd rotationPoint =
            gmtl::makeRot< gmtl::Quatd >( selectedMatrix );
        nae.SetAnimationEndPoints( navToPoint, rotationPoint );

        break;
    }
    case REMOVE_ALL_CAMERA_OBJECTS:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( activeCameraObject )
        {
            deviceHandler.UnselectObjects();
            cameraManager.SetActiveCameraObject( NULL );
        }

        cameraManager.removeChildren();

        break;
    }
    case DELETE_CAMERA_OBJECT:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        deviceHandler.UnselectObjects();
        cameraManager.removeChild( activeCameraObject );

        cameraManager.SetActiveCameraObject( NULL );

        break;
    }
    case SAVE_CAMERA_IMAGE:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        std::string saveImageDir;
        command->GetDataValuePair(
            "saveImageDirectory" )->GetData( saveImageDir );

        activeCameraObject->WriteImageFile( saveImageDir );

        break;
    }
    case SAVE_ALL_CAMERA_IMAGES:
    {
        std::string saveImageDir;
        command->GetDataValuePair(
            "saveImageDirectory" )->GetData( saveImageDir );

        cameraManager.WriteAllImageFiles( saveImageDir );

        break;
    }
    case TOGGLE_HIGHLIGHT_TOOL:
    {
        /*
        std::string name;
        command->GetDataValuePair( "addCameraObject" )->GetData( name );

        cameraManager.addChild( name );
        */

        //break;
    }
    case SELECT_MARKER_OBJECT:
    {
        /*
        unsigned int selection;
        command->GetDataValuePair(
            "selectCameraObject" )->GetData( selection );

        deviceHandler.UnselectObjects();

        scenegraph::camera::CameraObject* cameraObject =
            cameraManager.ConvertNodeToCameraObject(
                cameraManager.getChild( selection ) );

        cameraManager.SetActiveCameraObject( cameraObject );

        //Right now we are saying you must have a DCS
        scenegraph::DCS& selectedDCS = cameraObject->GetDCS();
        gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();

        //Set the connection between the scene manipulator and the selected dcs
        sceneManipulator->Connect( &selectedDCS );

        //If dcs is from a camera object, we want to rotate about local zero point
        osg::Vec3d center( 0.0, 0.0, 0.0 );
        center = center * osg::Matrixd( selectedMatrix.mData );
        sceneManipulator->SetPosition( center );

        //We need to transform center point into camera space
        //In the future the center point will be in world coordinates
        center = center * osg::Matrixd( sceneManager.GetWorldDCS()->GetMat().mData );
        gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
        deviceHandler.SetCenterPoint( &tempCenter );

        //Set the selected DCS
        deviceHandler.SetSelectedDCS( &selectedDCS );

        //Need to do this for multi-pass techniques
        if( sceneManager.IsRTTOn() )
        {
            selectedDCS.SetTechnique( "Glow" );
        }
        else
        {
            selectedDCS.SetTechnique( "Select" );
        }

        //Hand the node we are interested in off to the animation engine
        NavigationAnimationEngine& nae =
            *(NavigationAnimationEngine::instance());
        nae.SetDCS( sceneManager.GetWorldDCS() );

        //Hand our created end points off to the animation engine
        selectedMatrix = gmtl::invert( selectedMatrix );
        gmtl::Vec3d navToPoint =
            gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
        gmtl::Quatd rotationPoint =
            gmtl::makeRot< gmtl::Quatd >( selectedMatrix );
        nae.SetAnimationEndPoints( navToPoint, rotationPoint );
        */

        break;
    }
    case REMOVE_ALL_MARKER_OBJECTS:
    {
        //cameraManager.removeChildren();

        //break;
    }
    case DELETE_MARKER_OBJECT:
    {
        scenegraph::highlight::CircleHighlight* const activeCircleHighlight =
            highlightManager.GetActiveCircleHighlight();

        if( !activeCircleHighlight )
        {
            return;
        }

        highlightManager.removeChild( activeCircleHighlight );
        highlightManager.SetActiveCircleHighlight( NULL );

        break;
    }
    case DEPTH_OF_FIELD_EFFECT_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair(
            "depthOfFieldEffectOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        //mCameraEntity->DisplayDepthOfFieldEffect( onOff );

        break;
    }
    case PROJECTION_EFFECT_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair(
            "projectionEffectOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        //mCameraEntity->DisplayProjectionEffect( onOff );

        break;
    }
    case PROJECTION_EFFECT_OPACITY:
    {
        double value = 0;
        command->
            GetDataValuePair( "projectionEffectOpacity" )->GetData( value );

        //mCameraEntity->SetProjectionEffectOpacity( value );

        break;
    }
    case CAMERA_WINDOW_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "cameraWindowOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        //mCameraEntity->DisplayCameraViewQuad( onOff );
        osg::ref_ptr< osg::Group > viewCameraGroup;
        if( sceneManager.IsDesktopMode() )
        {
            viewCameraGroup = ves::xplorer::EnvironmentHandler::instance()->
                GetHeadsUpDisplay()->GetCamera();
        }
        else
        {
            viewCameraGroup = sceneManager.GetModelRoot();
        }

        if( onOff )
        {
            viewCameraGroup->addChild( cameraManager.GetCameraManagerQuad() );
        }
        else
        {
            viewCameraGroup->removeChild(
                cameraManager.GetCameraManagerQuad() );
        }
        break;
    }
    case CAMERA_WINDOW_RESOLUTION:
    {
        unsigned int value = 0;
        command->GetDataValuePair(
            "cameraWindowResolution" )->GetData( value );

        cameraManager.SetCameraViewQuadResolution( value );

        break;
    }
    case DEPTH_HELPER_WINDOW_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair(
            "depthHelperWindowOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        //mCameraEntity->DisplayDepthHelperQuad( onOff );

        break;
    }
    case DEPTH_HELPER_WINDOW_RESOLUTION:
    {
        unsigned int value = 0;
        command->GetDataValuePair(
            "depthHelperWindowResolution" )->GetData( value );

        //mCameraEntity->SetDepthHelperQuadResolution( value );

        break;
    }
    case CAMERA_GEOMETRY_ON_OFF:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        unsigned int selection = 0;
        command->GetDataValuePair(
            "cameraGeometryOnOff" )->GetData( selection );

        bool show = ( selection != 0 );
        activeCameraObject->ShowCameraGeometry( show );

        break;
    }
    case FRUSTUM_GEOMETRY_ON_OFF:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        unsigned int selection = 0;
        command->GetDataValuePair(
            "frustumGeometryOnOff" )->GetData( selection );

        bool show = ( selection != 0 );
        activeCameraObject->ShowFrustumGeometry( show );

        break;
    }
    case PROJECTION_UPDATE:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        double projectionData[ 4 ] = { 0, 0, 0, 0 };
        command->GetDataValuePair(
            "projectionFieldOfView" )->GetData( projectionData[ 0 ] );
        command->GetDataValuePair(
            "projectionAspectRatio" )->GetData( projectionData[ 1 ] );
        command->GetDataValuePair(
            "projectionNearPlane" )->GetData( projectionData[ 2 ] );
        command->GetDataValuePair(
            "projectionFarPlane" )->GetData( projectionData[ 3 ] );

        activeCameraObject->GetCamera().setProjectionMatrixAsPerspective(
            projectionData[ 0 ], projectionData[ 1 ],
            projectionData[ 2 ], projectionData[ 3 ] );

        activeCameraObject->Update();

        break;
    }
    case FOCAL_DISTANCE:
    {
        double value = 0;
        command->GetDataValuePair(
            "focalDistance" )->GetData( value );

        //mCameraEntity->SetFocalDistance( value );

        break;
    }
    case FOCAL_RANGE:
    {
        double value = 0;
        command->GetDataValuePair(
            "focalRange" )->GetData( value );

        //mCameraEntity->SetFocalRange( value );

        break;
    }
    case MAX_CIRCLE_OF_CONFUSION:
    {
        double value = 0;
        command->GetDataValuePair(
            "maxCircleOfConfusion" )->GetData( value );

        //mCameraEntity->SetMaxCircleOfConfusion( value );

        break;
    }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
