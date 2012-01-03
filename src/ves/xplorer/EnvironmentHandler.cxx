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
#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/device/cfdCursor.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SlotWrapper.h>

#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/environment/cfdTeacher.h>
#include <ves/xplorer/environment/cfdQuatCamHandler.h>
#include <ves/xplorer/environment/NavigationAnimationEngine.h>
#include <ves/xplorer/environment/cfdDisplaySettings.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/data/SeedPointActivateEH.h>
#include <ves/xplorer/event/data/SPBoundEH.h>
#include <ves/xplorer/event/data/SPDimensionsEH.h>
#include <ves/xplorer/event/data/SeedPoints.h>
#include <ves/xplorer/event/device/ChangeCursorEventHandler.h>
#include <ves/xplorer/event/environment/StoredSceneEH.h>
#include <ves/xplorer/event/environment/ChangeWorkingDirectoryEventHandler.h>
#include <ves/xplorer/event/environment/ChangeBackgroundColorEventHandler.h>
#include <ves/xplorer/event/environment/DisplayEventHandler.h>
#include <ves/xplorer/event/environment/ViewEventHandler.h>
#include <ves/xplorer/event/environment/ExportDOTFileEventHandler.h>
#include <ves/xplorer/event/environment/EphemerisDataEventHandler.h>
#include <ves/xplorer/event/environment/EphemerisAutoDateTimeEventHandler.h>
#include <ves/xplorer/event/environment/EphemerisHeightMapEventHandler.h>
#include <ves/xplorer/event/environment/EphemerisDisplayToggleEventHandler.h>
#include <ves/xplorer/event/environment/GeometryLODScaleEventHandler.h>
#include <ves/xplorer/event/environment/SetResetStartPositionEventHandler.h>
#include <ves/xplorer/event/environment/ManipulatorEventHandler.h>
#include <ves/xplorer/event/environment/CharacterEventHandler.h>
#include <ves/xplorer/event/environment/PhysicsEventHandler.h>
#include <ves/xplorer/event/environment/ScreenAlignedNormalsEventHandler.h>
#include <ves/xplorer/event/environment/CameraPlacementEventHandler.h>

#include <ves/xplorer/event/environment/EnvironmentSlots.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <osg/Vec3f>
#include <osg/BoundingSphere>
#include <osg/Group>

#include <osgText/Text>

/// C/C++ libraries
#include <fstream>
#include <cstdlib>

vprSingletonImpLifetime( ves::xplorer::EnvironmentHandler, 1 );

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::util;
using namespace ves::xplorer::command;

namespace ves
{
namespace xplorer
{
EnvironmentHandler::EnvironmentHandler()
    :
    _teacher( 0 ),
    mHeadsUpDisplay( 0 ),
    _activeGeomPicking( false ),
    cursor( 0 ),
    arrow( 0 ),
    displaySettings( 0 ),
    desktopWidth( 0 ),
    desktopHeight( 0 ),
    m_lodScale( 0.01 ),
    framerate( 0 )
{
    for( unsigned int i = 0; i < 3; i++ )
    {
        worldScale[ i ] = 1.0f;
        worldTrans[ i ] = 0.0f;
        worldRot[ i ] = 0.0f;
    }

    ///create seed points drawable
    _seedPoints = new ves::xplorer::SeedPoints( 4, 4, 1 );
    _seedPoints->Toggle( false );

    ///add a transform for manipulation of the seed points
    ///to sync with the active dataset
    _seedPointsDCS = new ves::xplorer::scenegraph::DCS();
    _seedPointsDCS->SetName( "Seed Points DCS" );
    _seedPointsDCS->addChild( _seedPoints.get() );

    _eventHandlers[ std::string( "MANIPULATOR_COMMAND" ) ] =
        new ves::xplorer::event::ManipulatorEventHandler();
    _eventHandlers[ std::string( "CHARACTER_COMMAND" ) ] =
        new ves::xplorer::event::CharacterEventHandler();
    _eventHandlers[ std::string( "PHYSICS_COMMAND" ) ] =
        new ves::xplorer::event::PhysicsEventHandler();
    _eventHandlers[ std::string( "VIEW_SELECTION" ) ] =
        new ves::xplorer::event::ViewEventHandler();
    //_eventHandlers[ std::string( "VISUALIZATION_SETTINGS" ) ] =
    //    new ves::xplorer::event::ChangeCursorEventHandler();
    _eventHandlers[ std::string( "Stored Scenes" ) ] =
        new ves::xplorer::event::StoredSceneEventHandler();
    _eventHandlers[ std::string( "Change Working Directory" ) ] =
        new ves::xplorer::event::ChangeWorkingDirectoryEventHandler();
    _eventHandlers[ std::string( "CHANGE_BACKGROUND_COLOR" ) ] =
        new ves::xplorer::event::ChangeBackgroundColorEventHandler();
    _eventHandlers[ std::string( "DISPLAY_SELECTION" ) ] =
        new ves::xplorer::event::DisplayEventHandler();
    _eventHandlers[ std::string( "Display Seed Points" ) ] =
        new ves::xplorer::event::SeedPointActivateEventHandler();
    _eventHandlers[ std::string( "Seed Points Bounds" ) ] =
        new ves::xplorer::event::SeedPointBoundsEventHandler();
    _eventHandlers[ std::string( "Seed Points Dimensions" ) ] =
        new ves::xplorer::event::SeedPointDimensionsEventHandler();
    _eventHandlers[ std::string( "DOT_FILE" ) ] =
        new ves::xplorer::event::ExportDOTFileEventHandler();
    _eventHandlers[ std::string( "Ephemeris Data" ) ] =
        new ves::xplorer::event::EphemerisDataEventHandler();
    _eventHandlers[ std::string( "Ephemeris Auto Date and Time" ) ] =
        new ves::xplorer::event::EphemerisAutoDateTimeEventHandler();
    _eventHandlers[ std::string( "Ephemeris Height Map" ) ] =
        new ves::xplorer::event::EphemerisHeightMapEventHandler();
    _eventHandlers[ std::string( "Ephemeris Toggle" ) ] =
        new ves::xplorer::event::EphemerisDisplayToggleEventHandler();
    _eventHandlers[ std::string( "Update LOD Scale" ) ] =
        new ves::xplorer::event::GeometryLODScaleEventHandler();
    _eventHandlers[ std::string( "Navigation_Data" ) ] =
        new ves::xplorer::event::SetResetStartPositionEventHandler();
    _eventHandlers[ std::string( "SCENE_STATE_INFORMATION" ) ] =
        new ves::xplorer::event::ScreenAlignedNormalsEventHandler();
    ///CPT Tool
    _eventHandlers[ "ADD_CAMERA_OBJECT" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "SELECT_CAMERA_OBJECT" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CHANGE_CAMERA_OBJECT_NAME" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "DELETE_CAMERA_OBJECT" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "REMOVE_ALL_CAMERA_OBJECTS" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "SAVE_CAMERA_IMAGE" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "SAVE_ALL_CAMERA_IMAGES" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "TOGGLE_HIGHLIGHT_TOOL" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "SELECT_MARKER_OBJECT" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CHANGE_MARKER_OBJECT_NAME" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "DELETE_MARKER_OBJECT" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "REMOVE_ALL_MARKER_OBJECTS" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "PROJECTION_EFFECT_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "PROJECTION_EFFECT_OPACITY" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CAMERA_WINDOW_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CAMERA_WINDOW_RESOLUTION" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "DEPTH_HELPER_WINDOW_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CAMERA_GEOMETRY_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "FRUSTUM_GEOMETRY_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "PROJECTION_UPDATE" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "FOCAL_DISTANCE" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "FOCAL_RANGE" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "MAX_CIRCLE_OF_CONFUSION" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "AUTO_COMPUTER_NEAR_FAR_PLANE" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CAMERA_MANAGER_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "PICTURE_ON_OFF" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();
    _eventHandlers[ "CHANGE_IMAGE_DIRECTORY" ] =
        new ves::xplorer::event::environment::CameraPlacementEventHandler();

    CONNECTSIGNALS_STATIC( "%PhysicsDebugger", void( bool const& enable ),
                     &ves::xplorer::event::environment::EnablePhysicsDebugging,
                     m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_STATIC( "%DisplayFrameRate", void( bool const& enable ),
                     &ves::xplorer::event::environment::DisplayFrameRate,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%DisplayGlobalAxis", void( bool const& enable ),
                     &ves::xplorer::event::environment::DisplayCoordinateSystem,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_STATIC( "%AmbientAudioSoundFile", void( std::string const& filename ),
                          &ves::xplorer::event::environment::SetAmbientAudioFile,
                          m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_STATIC( "%UsePreferredBackgroundColor",
                     void( bool const enable, std::vector< double > const& color ),
                     &ves::xplorer::event::environment::UpdateBackgroundColor,
                     m_connections, any_SignalType, normal_Priority );    
    
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::Initialize()
{
    displaySettings = new cfdDisplaySettings();

    arrow = ModelHandler::instance()->GetArrow();
}
////////////////////////////////////////////////////////////////////////////////
EnvironmentHandler::~EnvironmentHandler()
{
    if( cursor )
    {
        delete cursor;
    }

    if( _teacher )
    {
        delete _teacher;
    }

    if( displaySettings )
    {
        delete displaySettings;
    }

    if( mHeadsUpDisplay )
    {
        delete mHeadsUpDisplay;
    }
}
/////////////////////////////////////////////////////////////
void EnvironmentHandler::SetGlobalLODScale( double lodScale )
{
    m_lodScale = lodScale;
}
//////////////////////////////////////////////
double EnvironmentHandler::GetGlobalLODScale()
{
    return m_lodScale;
}

////////////////////////////////////////////////////////////////////////////////
/*cfdSoundHandler* EnvironmentHandler::GetSoundHandler()
{
   return _soundHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdTeacher* EnvironmentHandler::GetTeacher()
{
    return _teacher;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdQuatCamHandler* EnvironmentHandler::GetQuatCamHandler()
{
   return _camHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdDisplaySettings* EnvironmentHandler::GetDisplaySettings()
{
    return displaySettings;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::device::cfdCursor* EnvironmentHandler::GetCursor()
{
    return cursor;
}
////////////////////////////////////////////////////////////////////////////////
HeadsUpDisplay* EnvironmentHandler::GetHeadsUpDisplay()
{
    return mHeadsUpDisplay;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetDesktopSize( int width, int height )
{
    desktopWidth = width;
    desktopHeight = height;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::GetDesktopSize( int &width, int &height )
{
    width = desktopWidth;
    height = desktopHeight;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::InitScene()
{
    std::cout << 
        "| ***************************************************************** |" 
        << std::endl;

    //
    // Initiate quatcam
    //
    ves::xplorer::cfdQuatCamHandler::instance()->SetDCS( 
        ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    
    //
    // Initiate the Performer Stored Binary objects.
    //
    std::cout << 
        "| Initializing....................................... Stored Scenes |" 
        << std::endl;
    _teacher = new cfdTeacher( std::string( "STORED_FILES" ),
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );

    if( (desktopWidth > 0) && (desktopHeight > 0) )
    {
        std::cout << 
            "| Initializing....................................  Desktop Display |" 
            << std::endl;
        // Create the command and data value pairs
        // to adjust the desktop settings.
        ves::open::xml::DataValuePairPtr dvpDesktopWidth( new ves::open::xml::DataValuePair( std::string( "FLOAT" ) ) );
        dvpDesktopWidth->SetDataName( "desktop_width" );
        dvpDesktopWidth->SetDataValue( static_cast< double >( desktopWidth ) );
        
        ves::open::xml::DataValuePairPtr dvpDesktopHeight( new ves::open::xml::DataValuePair( std::string( "FLOAT" ) ) );
        dvpDesktopHeight->SetDataName( "desktop_height" );
        dvpDesktopHeight->SetDataValue( static_cast< double >( desktopHeight ) );
        
        ves::open::xml::CommandPtr displayCommand( new ves::open::xml::Command() );
        displayCommand->SetCommandName( std::string( "Juggler_Desktop_Data" ) );
        displayCommand->AddDataValuePair( dvpDesktopWidth );
        displayCommand->AddDataValuePair( dvpDesktopHeight );
        displaySettings->SetVECommand( displayCommand );
        //displaySettings->ProcessCommand();
    }

    //
    // Initialize HeadsUpDisplay
    //
    std::pair< int, int > screenDims = displaySettings->GetScreenResolution();

    std::cout 
        << "| Initializing.................................... Heads Up Display |" << std::endl;
    mHeadsUpDisplay = new ves::xplorer::scenegraph::HeadsUpDisplay( screenDims );
}
////////////////////////////////////////////////////////////////////////////////
//This function sets the dcs based on any input device
//(i.e) trackball, wand, gui nav,...
void EnvironmentHandler::PreFrameUpdate()
{
    //Process all events for active device
    ///With signals we do not need to explicitly call into device handler
    ///to get the devices to execute nav updates
    /*if( !ves::xplorer::NavigationAnimationEngine::instance()->IsActive() )
    {
        ves::xplorer::DeviceHandler::instance()->ProcessDeviceEvents();
    }*/

    ves::xplorer::cfdQuatCamHandler::instance()->PreFrameUpdate();
    ves::xplorer::NavigationAnimationEngine::instance()->PreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::LatePreFrameUpdate()
{
    // Update Navigation variables
    vprDEBUG( vesDBG, 3 ) << "|\tEnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;

    const ves::open::xml::CommandPtr tempCommand = CommandManager::instance()->GetXMLCommand();
    if( tempCommand )
    {
        const std::string commandName = tempCommand->GetCommandName();
        std::map<std::string, ves::xplorer::event::EventHandler*>::const_iterator currentEventHandler;
        currentEventHandler = _eventHandlers.find( commandName );
        if( currentEventHandler != _eventHandlers.end() )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tEnvironmentHandler::LatePreFrameUpdate Executing: "
                << commandName
                << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tempCommand );
        }
    }

    mHeadsUpDisplay->SetFrameRate( framerate );
    mHeadsUpDisplay->LatePreFrame();
    vprDEBUG( vesDBG, 3 ) << "|\tEnd EnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetWindowDimensions( unsigned int w, unsigned int h )
{
    _windowWidth = w;
    _windowHeight = h;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int EnvironmentHandler::GetWindowWidth()
{
    return _windowWidth;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int EnvironmentHandler::GetWindowHeight()
{
    return _windowHeight;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetFrameRate( float value )
{
    framerate = value;
}
////////////////////////////////////////////////////////////////////////////////
float EnvironmentHandler::GetFrameRate()
{
    return framerate;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::PostFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::SeedPoints* EnvironmentHandler::GetSeedPoints()
{
    if( _seedPoints.valid() )
    {
        return _seedPoints.get();
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* EnvironmentHandler::GetSeedPointsDCS()
{
    if( _seedPointsDCS.valid() )
    {
        return _seedPointsDCS.get();
    }
    return 0;
}
/////////////////////////////////////////////////////////////////////////////////////////////////
osgEphemeris::EphemerisModel* EnvironmentHandler::GetEphemerisModel( bool createIfDoesNotExist )
{
    if( !m_ephemerisModel.valid() && createIfDoesNotExist )
    {
        m_ephemerisModel = new osgEphemeris::EphemerisModel();
        osg::ref_ptr<osg::Group> worldDCS =
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot();

        /*if(worldDCS->getBound().valid())
        {
            m_ephemerisModel->setSkyDomeRadius( worldDCS->getBound().radius()*2 );
            m_ephemerisModel->setSkyDomeCenter( worldDCS->getBound().center() );
         m_ephemerisModel->setMembers(osgEphemeris::EphemerisModel::ALL_MEMBERS);
        }
        else*/
        {
            m_ephemerisModel->setSkyDomeRadius( 10000.0 );
            m_ephemerisModel->setAutoDateTime( false );
            m_ephemerisModel->setSkyDomeCenter( osg::Vec3f( 0.0, 0.0, 0.0 ) );
            m_ephemerisModel->setSunLightNum( 0 );
            m_ephemerisModel->setMoveWithEyePoint( false );
            m_ephemerisModel->setMembers(
                osgEphemeris::EphemerisModel::DEFAULT_MEMBERS );
        }
        worldDCS->addChild( m_ephemerisModel.get() );
    }
    return m_ephemerisModel.get();
}
////////////////////////////////////////////////////////////////////////////////
} // end xplorer
} // end ves
