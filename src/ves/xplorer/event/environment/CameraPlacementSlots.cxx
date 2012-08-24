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

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/environment/CameraPlacementSlots.h>

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

#include <ves/xplorer/data/CameraSettingsPropertySet.h>
#include <ves/xplorer/data/CameraModePropertySet.h>

#include <osgDB/FileUtils>

#include <iostream>

// --- VRJ Includes --- //
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

#include <boost/concept_check.hpp>

#include <Poco/Path.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace environment
{

void DisableCameraTools( bool flag )
{
    //Need to turn off camerawindow since cameras will get garbage as
    //texture
    if( flag )
    {
        CameraWindowOn( false );
    }
    else
    {
        //Load CameraModePropertySet, check the state of CameraWindow, and
        //take appropriate action
        propertystore::PropertySetPtr set =
                propertystore::PropertySetPtr( new ves::xplorer::data::CameraModePropertySet );
        //See CameraModePropertySet for explanation of this uuid
        set->SetUUID( "00000000-0101-1010-1111-000011110000" );
        set->Load();
        bool cwo = boost::any_cast<bool>( set->GetPropertyValue( "CameraWindow" ) );
        CameraWindowOn( cwo );
    }

    //Slot is "disable", call is "enable", so negate flag.
    scenegraph::SceneManager::instance()->GetCameraManager().Enable( !flag );
}
////////////////////////////////////////////////////////////////////////////////
void AddCamera( const std::string& uuid, const std::string& name )
{
    scenegraph::SceneManager::instance()->GetCameraManager().addChild( name, uuid );

    propertystore::PropertySetPtr set =
            propertystore::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    set->SetUUID( uuid );
    set->Load();

    scenegraph::camera::CameraObject* cameraObject = scenegraph::SceneManager::instance()->GetCameraManager().GetCameraObject( uuid );
    bool flag = boost::any_cast< bool >( set->GetPropertyValue( "Projection_AutoComputeFarPlane" ) );
    cameraObject->ComputeNearFarPlanes( flag );

    flag = boost::any_cast< bool >( set->GetPropertyValue( "ShowCameraGeometry" ) );
    cameraObject->ShowCameraGeometry( flag );

    flag = boost::any_cast< bool >( set->GetPropertyValue( "ShowFrustumGeometry" ) );
    cameraObject->ShowFrustumGeometry( flag );

    cameraObject->Update();
}
////////////////////////////////////////////////////////////////////////////////
void RemoveCamera( const std::string& uuid )
{
    DeviceHandler& deviceHandler = *( DeviceHandler::instance() );
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();
    scenegraph::camera::CameraManager& cameraManager =
        sceneManager.GetCameraManager();
    scenegraph::camera::CameraObject* const cameraObject =
            cameraManager.GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    deviceHandler.UnselectObjects();
    cameraManager.removeChild( cameraObject );

    cameraManager.SetActiveCameraObject( NULL );
 }
////////////////////////////////////////////////////////////////////////////////
void SelectCamera( const std::string& uuid )
{
    DeviceHandler& deviceHandler = *( DeviceHandler::instance() );
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();
    scenegraph::highlight::HighlightManager& highlightManager =
        sceneManager.GetHighlightManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        sceneManager.GetManipulatorManager().GetSceneManipulator();
    scenegraph::camera::CameraManager& cameraManager =
        sceneManager.GetCameraManager();

    deviceHandler.UnselectObjects();

    scenegraph::camera::CameraObject* newCameraObject =
            cameraManager.GetCameraObject( uuid );

    if( !newCameraObject )
    {
        return;
    }

    cameraManager.SetActiveCameraObject( newCameraObject );

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
}
////////////////////////////////////////////////////////////////////////////////
void BeginFlythrough( const std::vector< std::string >& cameraUUIDList )
{
    if( cameraUUIDList.empty() )
    {
        return;
    }

    // Convert camera uuids into a vector of paired vec3s and quaternions
    std::vector < std::pair < gmtl::Vec3d, gmtl::Quatd > > animPointList;
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();
    scenegraph::camera::CameraManager& cameraManager =
        sceneManager.GetCameraManager();
    scenegraph::camera::CameraObject* cameraObject;
    std::vector< std::string >::const_iterator itr = cameraUUIDList.begin();
    while( itr != cameraUUIDList.end() )
    {
        cameraObject = cameraManager.GetCameraObject( *itr );
        if( cameraObject )
        {
            scenegraph::DCS& selectedDCS = cameraObject->GetDCS();
            gmtl::Matrix44d selectedMatrix = selectedDCS.GetMat();
            selectedMatrix = gmtl::invert( selectedMatrix );
            const gmtl::Matrix44d tempHeadMatrix = sceneManager.GetHeadMatrix();
            const gmtl::AxisAngled myAxisAngle( gmtl::Math::deg2Rad( double( -90 ) ), 1, 0, 0 );
            gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
            selectedMatrix = tempHeadMatrix * myMat * selectedMatrix;

            gmtl::Vec3d navToPoint =
                gmtl::makeTrans< gmtl::Vec3d >( selectedMatrix );
            gmtl::Quatd rotationPoint =
                gmtl::makeRot< gmtl::Quatd >( selectedMatrix );

            std::pair < gmtl::Vec3d, gmtl::Quatd > animPoint( navToPoint, rotationPoint );
            animPointList.push_back( animPoint );
        }
        ++itr;
    }

    NavigationAnimationEngine& nae =
        *( NavigationAnimationEngine::instance() );
    nae.SetDCS( sceneManager.GetNavDCS() );
    nae.SetAnimationPoints( animPointList );
}
////////////////////////////////////////////////////////////////////////////////
void EndFlythrough()
{
    NavigationAnimationEngine::instance()->StopAnimation();
}
////////////////////////////////////////////////////////////////////////////////
void LoopFlythrough( bool flag )
{
    NavigationAnimationEngine::instance()->SetAnimationLoopingOn( flag );
}
////////////////////////////////////////////////////////////////////////////////
void SetFlythroughSpeed( double speed )
{
    if( speed > 0 )
    {
        NavigationAnimationEngine::instance()->SetAnimationSpeed( speed );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraManagerOn( bool flag )
{
    scenegraph::SceneManager::instance()->GetCameraManager().EnableCPT( flag );
}
////////////////////////////////////////////////////////////////////////////////
void CameraWindowOn( bool flag )
{
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();
    scenegraph::camera::CameraManager& cameraManager =
        sceneManager.GetCameraManager();

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

    if( flag )
    {
        viewCameraGroup->addChild( cameraManager.GetCameraManagerQuad() );
    }
    else
    {
        viewCameraGroup->removeChild(
            cameraManager.GetCameraManagerQuad() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraWindowResolution( int resolution )
{
    scenegraph::SceneManager::instance()->GetCameraManager().
            SetCameraViewQuadResolution( resolution );
}
////////////////////////////////////////////////////////////////////////////////
void PictureModeOn( bool flag )
{
    scenegraph::SceneManager::instance()->GetCameraManager().SetPictureMode( flag );
}
////////////////////////////////////////////////////////////////////////////////
std::string ResolveSaveImagePath( std::string path )
{
    if( path.empty() )
    {
        propertystore::PropertySetPtr set =
                propertystore::PropertySetPtr( new ves::xplorer::data::CameraModePropertySet );
        //See CameraModePropertySet for explanation of this uuid
        set->SetUUID( "00000000-0101-1010-1111-000011110000" );
        set->Load();
        path = boost::any_cast<std::string>( set->GetPropertyValue( "CameraImageSavePath" ) );
    }

    Poco::Path imagePath( path );
    imagePath.setFileName( "" );
    imagePath.makeAbsolute();

    return imagePath.toString(Poco::Path::PATH_UNIX);
}
////////////////////////////////////////////////////////////////////////////////
void SaveCameraImage( const std::string& uuid, const std::string& path )
{
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();

    if( !sceneManager.IsMasterNode() )
    {
        return;
    }

    std::string saveImageDir = ResolveSaveImagePath( path );

    // Setting the uuid to "PICTURE_MODE" lets us know this is not normal camera
    // that has been explicitly placed in the scene. CameraManager knows how to deal
    // with this when picture mode is turned on.
    if( uuid == ("PICTURE_MODE") )
    {
        sceneManager.GetCameraManager().WriteActiveCameraImageFile( saveImageDir );
    }
    else
    {
        sceneManager.GetCameraManager().WriteCameraImageFile( uuid, saveImageDir );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SaveAllCameraImages( const std::string& path )
{
    scenegraph::SceneManager& sceneManager =
        *scenegraph::SceneManager::instance();

    if( !sceneManager.IsMasterNode() )
    {
        return;
    }

    std::string saveImageDir = ResolveSaveImagePath( path );

    sceneManager.GetCameraManager().WriteAllImageFiles( saveImageDir );
}
////////////////////////////////////////////////////////////////////////////////
void ChangeCameraName( const std::string& uuid, const std::string& newName )
{
    scenegraph::camera::CameraObject* const cameraObject =
            scenegraph::SceneManager::instance()->GetCameraManager().
            GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    cameraObject->setName( newName );
}
////////////////////////////////////////////////////////////////////////////////
void CameraAutoComputeFarPlane( const std::string& uuid, bool flag )
{
    scenegraph::camera::CameraObject* const cameraObject =
            scenegraph::SceneManager::instance()->GetCameraManager().
            GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    cameraObject->ComputeNearFarPlanes( flag );

    cameraObject->Update();
}
////////////////////////////////////////////////////////////////////////////////
void ShowCameraGeometry( const std::string& uuid, bool flag )
{
    scenegraph::camera::CameraObject* const cameraObject =
            scenegraph::SceneManager::instance()->GetCameraManager().
            GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    cameraObject->ShowCameraGeometry( flag );
}
////////////////////////////////////////////////////////////////////////////////
void ShowCameraFrustumGeometry( const std::string& uuid, bool flag )
{
    scenegraph::camera::CameraObject* const cameraObject =
            scenegraph::SceneManager::instance()->GetCameraManager().
            GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    cameraObject->ShowFrustumGeometry( flag );
}
////////////////////////////////////////////////////////////////////////////////
void CameraFocalDistance( const std::string& uuid, double focalDistance )
{
    // Wasn't hooked up in old code; not sure of status.

    //mCameraEntity->SetFocalDistance( value );
}
////////////////////////////////////////////////////////////////////////////////
void CameraFocalRange( const std::string& uuid, double focalRange )
{
    // Wasn't hooked up in old code; not sure of status.

    //mCameraEntity->SetFocalRange( value );
}
////////////////////////////////////////////////////////////////////////////////
void CameraMaxCircleOfConfusion( const std::string& uuid, double maxCircle )
{
    // Wasn't hooked up in old code; not sure of status.

    //mCameraEntity->SetMaxCircleOfConfusion( value );
}
////////////////////////////////////////////////////////////////////////////////
void CameraProjectionUpdate( const std::string& uuid )
{
    scenegraph::camera::CameraObject* const cameraObject =
            scenegraph::SceneManager::instance()->GetCameraManager().
            GetCameraObject( uuid );

    if( !cameraObject )
    {
        return;
    }

    propertystore::PropertySetPtr set =
            propertystore::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    set->SetUUID( uuid );
    set->Load();

    //Get the ImageDimensions string, which is formatted as "widthxheight label"
    //and split it at "x" and the space between "height" and "label" to pull
    //out the width and height
    std::string imageDims = boost::any_cast<std::string>(
                set->GetPropertyValue( "Projection_ImageDimensions" ) );
    size_t xloc = imageDims.find( 'x' );
    std::string strWidth( imageDims, 0, xloc );
    std::string strHeight( imageDims, xloc + 1, imageDims.find( ' ' ) - xloc - 1 );
    std::stringstream ss;
    unsigned int width;
    unsigned int height;
    ss << strWidth;
    ss >> width;
    std::stringstream s2;
    s2 << strHeight;
    s2 >> height;

    double fovz = boost::any_cast<double>( set->GetPropertyValue( "Projection_FOVZ" ) );
    double aspectRatio = static_cast<double>(width)/(height);
    double nearPlane = boost::any_cast<double>( set->GetPropertyValue( "Projection_NearPlane" ) );
    double farPlane = boost::any_cast<double>( set->GetPropertyValue( "Projection_FarPlane" ) );

    cameraObject->GetCamera().setProjectionMatrixAsPerspective(
        fovz, aspectRatio, nearPlane, farPlane );

    bool autoComputeFarPlane = boost::any_cast<bool>( set->GetPropertyValue( "Projection_AutoComputeFarPlane" ) );
    cameraObject->ComputeNearFarPlanes( autoComputeFarPlane );

    std::pair< unsigned int, unsigned int > resolution =
        std::make_pair< unsigned int, unsigned int >( width, height );
    cameraObject->SetTextureResolution( resolution );

    //Now update everything
    cameraObject->Update();
}
////////////////////////////////////////////////////////////////////////////////

// The remainder of functionality from CameraPlacementEventHandlers that needs
// to be lightly re-worked and implemented here.
/*

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




*/

////////////////////////////////////////////////////////////////////////////////
}}}} // ves::xplorer::event::environment
