/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <boost/concept_check.hpp>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::environment;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementEventHandler::CameraPlacementEventHandler()
    :
    EventHandler()
{
    ///REmember that any new event handlers need to be added
    ///to EnvironmentHandler too.
    mCommandNameToInt[ "ADD_CAMERA_OBJECT" ] =
        ADD_CAMERA_OBJECT;
    mCommandNameToInt[ "SELECT_CAMERA_OBJECT" ] =
        SELECT_CAMERA_OBJECT;
    mCommandNameToInt[ "CHANGE_CAMERA_OBJECT_NAME" ] =
        CHANGE_CAMERA_OBJECT_NAME;
    mCommandNameToInt[ "DELETE_CAMERA_OBJECT" ] =
        DELETE_CAMERA_OBJECT;
    mCommandNameToInt[ "REMOVE_ALL_CAMERA_OBJECTS" ] =
        REMOVE_ALL_CAMERA_OBJECTS;

    mCommandNameToInt[ "SAVE_CAMERA_IMAGE" ] =
        SAVE_CAMERA_IMAGE;
    mCommandNameToInt[ "SAVE_ALL_CAMERA_IMAGES" ] =
        SAVE_ALL_CAMERA_IMAGES;
    mCommandNameToInt[ "CHANGE_IMAGE_DIRECTORY" ] =
        CHANGE_IMAGE_DIRECTORY;

    mCommandNameToInt[ "TOGGLE_HIGHLIGHT_TOOL" ] =
        TOGGLE_HIGHLIGHT_TOOL;
    mCommandNameToInt[ "SELECT_MARKER_OBJECT" ] =
        SELECT_MARKER_OBJECT;
    mCommandNameToInt[ "CHANGE_MARKER_OBJECT_NAME" ] =
        CHANGE_MARKER_OBJECT_NAME;
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

    mCommandNameToInt[ "AUTO_COMPUTER_NEAR_FAR_PLANE" ] =
        AUTO_COMPUTER_NEAR_FAR_PLANE;
    mCommandNameToInt[ "CAMERA_MANAGER_ON_OFF" ] =
        CAMERA_MANAGER_ON_OFF;
    mCommandNameToInt[ "PICTURE_ON_OFF" ] =
        PICTURE_ON_OFF;
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
    ves::xplorer::event::EventHandler( ceh )
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

    //Set the active cameraObject once the command->GetCommandName()manager is created
    int commandName =
        mCommandNameToInt.find( command->GetCommandName() )->second;

    DeviceHandler& deviceHandler = *( DeviceHandler::instance() );
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

        if( commandName == ADD_CAMERA_OBJECT )
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "autoComputeNearFarPlane" )->GetData( selection );

            bool onOff = ( selection != 0 );
            cameraManager.GetActiveCameraObject()->ComputeNearFarPlanes( onOff );
        }

        //Right now we are saying you must have a DCS
        scenegraph::DCS& selectedDCS = newCameraObject->GetDCS();
        gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();

        //If dcs is from a camera object, we want to rotate about local zero point
        osg::Vec3d center( 0.0, 0.0, 0.0 );
        center = center * osg::Matrixd( selectedMatrix.mData );

        //Set the connection between the scene manipulator and the selected dcs
        if( sceneManager.GetManipulatorManager().IsEnabled() )
        {
            sceneManipulator->Connect( &selectedDCS );
            sceneManipulator->SetPosition( center );
        }

        if( sceneManager.IsDesktopMode() )
        {
            //We need to transform center point into camera space
            //In the future the center point will be in world coordinates
            center =
                center * osg::Matrixd( sceneManager.GetNavDCS()->GetMat().mData );
            gmtl::Point3d tempCenter( center.x(), center.y(), center.z() );
            deviceHandler.SetCenterPoint( &tempCenter );
        }

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

        if( sceneManager.IsDesktopMode() )
        {
            //Hand the node we are interested in off to the animation engine
            NavigationAnimationEngine& nae =
                *( NavigationAnimationEngine::instance() );
            nae.SetDCS( sceneManager.GetNavDCS() );

            //Hand our created end points off to the animation engine
            selectedMatrix = gmtl::invert( selectedMatrix );
            const gmtl::Matrix44d tempHeadMatrix = sceneManager.GetHeadMatrix();
            const gmtl::AxisAngled myAxisAngle( gmtl::Math::deg2Rad( double( -90 ) ), 1, 0, 0 );
            gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
            selectedMatrix = tempHeadMatrix * myMat * selectedMatrix;

            gmtl::Vec3d navToPoint =
                gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
            gmtl::Quatd rotationPoint =
                gmtl::makeRot< gmtl::Quatd >( selectedMatrix );
            nae.SetAnimationEndPoints( navToPoint, rotationPoint );
        }

        break;
    }
    case CHANGE_CAMERA_OBJECT_NAME:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        std::string name;
        command->GetDataValuePair( "changeCameraObjectName" )->GetData( name );
        activeCameraObject->setName( name );

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
        if( !sceneManager.IsMasterNode() )
        {
            return;
        }

        std::string saveImageDir;
        command->GetDataValuePair(
            "saveImageDirectory" )->GetData( saveImageDir );

        cameraManager.WriteActiveCameraImageFile( saveImageDir );

        break;
    }
    case SAVE_ALL_CAMERA_IMAGES:
    {
        if( !sceneManager.IsMasterNode() )
        {
            return;
        }
        std::string saveImageDir;
        command->GetDataValuePair(
            "saveImageDirectory" )->GetData( saveImageDir );


        cameraManager.WriteAllImageFiles( saveImageDir );

        break;
    }
    case CHANGE_IMAGE_DIRECTORY:
    {
        if( !sceneManager.IsMasterNode() )
        {
            return;
        }

        std::string saveImageDir;
        command->GetDataValuePair(
            "saveImageDirectory" )->GetData( saveImageDir );

        cameraManager.SetImageStoreDirectory( saveImageDir );

        break;
    }
    case TOGGLE_HIGHLIGHT_TOOL:
    {
        unsigned int value;
        command->GetDataValuePair( "toggleHighlightTool" )->GetData( value );

        bool toggle = static_cast< bool >( value );

        highlightManager.Toggle( toggle );

        break;
    }
    case SELECT_MARKER_OBJECT:
    {
        unsigned int selection;
        command->GetDataValuePair( "selectMarkerObject" )->GetData( selection );

        scenegraph::highlight::CircleHighlight* newCircleHighlight =
            highlightManager.ConvertNodeToCircleHighlight(
                highlightManager.getChild( selection ) );

        highlightManager.SetActiveCircleHighlight( newCircleHighlight );

        break;
    }
    case CHANGE_MARKER_OBJECT_NAME:
    {
        scenegraph::highlight::CircleHighlight* const activeCircleHighlight =
            highlightManager.GetActiveCircleHighlight();

        if( !activeCircleHighlight )
        {
            return;
        }

        std::string name;
        command->GetDataValuePair( "changeMarkerObjectName" )->GetData( name );
        activeCircleHighlight->setName( name );

        break;
    }
    case REMOVE_ALL_MARKER_OBJECTS:
    {
        scenegraph::highlight::CircleHighlight* const activeCircleHighlight =
            highlightManager.GetActiveCircleHighlight();

        if( activeCircleHighlight )
        {
            highlightManager.SetActiveCircleHighlight( NULL );
        }

        highlightManager.removeChildren();

        break;
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

        //bool onOff = ( selection != 0 );
        //cameraManager.GetActiveCameraObject()->
        //    DisplayDepthOfFieldEffect( onOff );
        break;
    }
    case CAMERA_MANAGER_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair(
            "cameraManagerOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        cameraManager.EnableCPT( onOff );
        break;
    }
    case AUTO_COMPUTER_NEAR_FAR_PLANE:
    {
        scenegraph::camera::CameraObject* const activeCameraObject =
            cameraManager.GetActiveCameraObject();

        if( !activeCameraObject )
        {
            return;
        }

        unsigned int selection = 0;
        command->GetDataValuePair(
            "autoComputeNearFarPlane" )->GetData( selection );

        bool onOff = ( selection != 0 );
        activeCameraObject->ComputeNearFarPlanes( onOff );

        activeCameraObject->Update();

        break;
    }
    case PROJECTION_EFFECT_ON_OFF:
    {
        unsigned int selection = 0;
        command->GetDataValuePair(
            "projectionEffectOnOff" )->GetData( selection );

        bool onOff = ( selection != 0 );
        cameraManager.DisplayProjectionEffect( onOff );

        break;
    }
    case PROJECTION_EFFECT_OPACITY:
    {
        double value = 0;
        command->GetDataValuePair(
            "projectionEffectOpacity" )->GetData( value );

        cameraManager.SetProjectionEffectOpacity( value );

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

        //bool onOff = ( selection != 0 );
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

        //Set the Project matrix
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

        //Set the near/far plane
        unsigned int selection = 0;
        command->GetDataValuePair(
            "autoComputeNearFarPlane" )->GetData( selection );

        bool onOff = ( selection != 0 );
        activeCameraObject->ComputeNearFarPlanes( onOff );

        //Setup the textture resolution
        unsigned int tempX = 0;
        unsigned int tempY = 0;

        command->GetDataValuePair(
            "projectionXImageResolution" )->GetData( tempX );
        command->GetDataValuePair(
            "projectionYImageResolution" )->GetData( tempY );
        std::pair< unsigned int, unsigned int > resolution =
            std::make_pair< unsigned int, unsigned int >( tempX, tempY );
        activeCameraObject->SetTextureResolution( resolution );

        //Now update everything
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
    case PICTURE_ON_OFF:
    {
        unsigned int value = 0;
        command->GetDataValuePair( "pictureModeOnOff" )->GetData( value );
        cameraManager.SetPictureMode( value );
        break;
    }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    boost::ignore_unused_variable_warning( modelHandler );
}
////////////////////////////////////////////////////////////////////////////////
