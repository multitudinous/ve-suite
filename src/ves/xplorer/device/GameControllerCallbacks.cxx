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
#include <ves/xplorer/device/GameControllerCallbacks.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/highlight/CircleHighlight.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/open/xml/model/Model.h>

#include <osgwMx/MxGamePad.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- vrJuggler Includes --- //
#include <vrj/vrjParam.h>

#include <jccl/RTRC/ConfigManager.h>

#include <vpr/System.h>

#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>

// --- STL Includes --- //
#include <iostream>
#include <cmath>

using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
GameControllerCallbacks::GameControllerCallbacks()
    :
    Device( KEYBOARD_MOUSE ),
    m_controlledState( OpenControlState ),
    m_activeController( 0 ),
    m_currX( 0 ),
    m_currY( 0 ),
    m_exit( false ),
    // create a game pad input handler and data interpreter to control the view.
    //m_gadgetInputAdapter( new osgwMx::MxInputAdapterGadgeteerGamePad() ),
    m_mxGamePadStyle( new osgwMx::MxGamePad() ),
    m_leftStickX( 0 ),
    m_leftStickY( 0 ),
    m_rightStickX( 0 ),
    m_rightStickY( 0 ),
    m_buttons( 0 ),
    m_buttonMap( new osgwMx::FunctionalMap() ),
    m_viewMatrix( ves::xplorer::scenegraph::SceneManager::instance()->GetMxCoreViewMatrix() ),
    m_success( false ),
    m_uiMode( false )
{
    ConfigureGameControllerDevices();

    // Create a default functional map.
    m_buttonMap->configure( osgwMx::MxGamePad::Button0, osgwMx::FunctionalMap::JumpToWorldOrigin );
    m_buttonMap->configure( osgwMx::MxGamePad::Button1, osgwMx::FunctionalMap::LevelView );
    m_buttonMap->configure( osgwMx::MxGamePad::Button2, osgwMx::FunctionalMap::MoveModifyUpDown );
    m_buttonMap->configure( osgwMx::MxGamePad::Button3, osgwMx::FunctionalMap::JumpToHomePosition );
    m_buttonMap->configure( osgwMx::MxGamePad::Button6, osgwMx::FunctionalMap::MoveModifyScaleSpeedDown );
    m_buttonMap->configure( osgwMx::MxGamePad::Button7, osgwMx::FunctionalMap::MoveModeOrbit );
    m_buttonMap->configure( osgwMx::MxGamePad::Button8, osgwMx::FunctionalMap::MoveModeLocal );
    m_buttonMap->configure( osgwMx::MxGamePad::Button9, osgwMx::FunctionalMap::MoveModeConstrained );
    m_buttonMap->configure( osgwMx::MxGamePad::Button10, osgwMx::FunctionalMap::RotateModeOrbit );
    m_buttonMap->configure( osgwMx::MxGamePad::Button11, osgwMx::FunctionalMap::RotateModeLocal );
    m_buttonMap->configure( osgwMx::MxGamePad::Button12, osgwMx::FunctionalMap::MoveUpAtRate );
    m_buttonMap->configure( osgwMx::MxGamePad::Button13, osgwMx::FunctionalMap::MoveDownAtRate );
    ///Initialize the gamepad class
    m_mxGamePadStyle->setFunctionalMap( m_buttonMap.get() );
    m_mxGamePadStyle->setStickRate( 5.0 );
    m_mxGamePadStyle->setStickDeadZone( 0.05f );
    m_mxGamePadStyle->setMxCore( &m_viewMatrix );

    //MoveModeLiteral - moves in opengl / eye space
    //MoveModeWorld - moves in rotate object space
    //MoveModeLocal - moves in global coordinates
    //MoveModeOriented - moves in the global coordinate system defined by the orientation of the game controller
    
    //MoveModeLiteral - Move in literal coordinates
    //MoveModeLocal - Move in eye local coordinates
    //MoveModeConstrained - Move in the ground plane (defined by the initial up vector)
    //MoveModeOriented - Move in the oriented coordinate system
    //MoveModeWorld - Move in world coordinates
    //MoveModeOrbit - Move along a vector between the eye and the orbit center, slowing when near the center and speeding up when further away

    //m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeLocal );
    //In MoveModeOriented when in desktop mode when the position data from VESJoystick
    //is not defined the nav is just like MoveModeLocal. In a tracked environment when
    //VESJoystick is defined the nav will be affected by the orientation of the controller.
    m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeOriented );

    //Setup rumble
    /*
     void RumbleApp::initRumble()
     {
     _rumble.init("Rumble0");
     
     _speed = _rumble->createEffect(RumbleEffect::SINE);
     if (_speed) {
     _speed->load();
     }
     else
     {
     std::cout << "RumbleEffect::SINE not supported" << std::endl;
     }
     
     _objectHit[0] = _rumble->createEffect(RumbleEffect::SQUARE);
     if (_objectHit[0]) {
     _objectHit[0]->setMagnitude(1);
     _objectHit[0]->setLength(5000);
     _objectHit[0]->load();
     }
     
     _objectHit[1] = _rumble->createEffect(RumbleEffect::SINE);
     if (_objectHit[1]) {
     _objectHit[1]->setMagnitude(0.25);
     _objectHit[1]->setLength(2000);
     _objectHit[1]->load();
     }
     
     _objectHit[2] = _rumble->createEffect(RumbleEffect::SINE);
     if (_objectHit[2]) {
     _objectHit[2]->setMagnitude(0.5);
     _objectHit[2]->setLength(500);
     _objectHit[2]->load();
     }
     
     _objectHit[3] = _rumble->createEffect(RumbleEffect::SINE);
     if (_objectHit[3]) {
     _objectHit[3]->setMagnitude(0.75);
     _objectHit[3]->setLength(2000);
     _objectHit[3]->load();
     }
     }
     if (_speed) {
     _speed->setMagnitude(1.0-_axes[1]->getData());
     _speed->update();
     _speed->play(-1);
     }
     
     for (int i = 0 ; i < 4 ; ++i)
     {
     if (_button[i]->getData() == gadget::Digital::TOGGLE_ON && _objectHit[i])
     {
     _objectHit[i]->play(1);
     std::cout << "\tButton " << i << " pressed" << std::endl;
     }
     }
     */
    CONNECTSIGNALS_1( "%NavigationRotationMode", void( std::string ),
                      &GameControllerCallbacks::SetRotationMode,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%CharacterUpdate", void( bool const& ),
                      &GameControllerCallbacks::SetCharacterState,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%GrabControllerState", void( unsigned int const& ),
                     &GameControllerCallbacks::GrabControlState,
                     m_connections, any_SignalType, normal_Priority );
    
    // Register signal(s) with EventManager
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_updateData ),
        "GameController.UpdateData" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_hideShowUI ),
        "GameController.HideShowUI" );

    m_gameControllerButtonSignalMap["GameController.Button0"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button1"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button2"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button3"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button4"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button5"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button6"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button7"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button8"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button9"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button10"] = new GameControllerButtonSignal_type;
    m_gameControllerButtonSignalMap["GameController.Button11"] = new GameControllerButtonSignal_type;

    m_gameControllerAnalogSignalMap["GameController.Axis0"] = new GameControllerAnalogSignal_type;
    m_gameControllerAnalogSignalMap["GameController.Axis1"] = new GameControllerAnalogSignal_type;
    m_gameControllerAnalogSignalMap["GameController.Axis2"] = new GameControllerAnalogSignal_type;
    m_gameControllerAnalogSignalMap["GameController.Axis3"] = new GameControllerAnalogSignal_type;
    m_gameControllerAnalogSignalMap["GameController.Axis4"] = new GameControllerAnalogSignal_type;
    m_gameControllerAnalogSignalMap["GameController.Axis5"] = new GameControllerAnalogSignal_type;

    // register "raw" button signals with EventManager
    for( GameControllerButtonSignalMap_type::const_iterator
            iter = m_gameControllerButtonSignalMap.begin();
            iter != m_gameControllerButtonSignalMap.end(); ++iter )
    {
        switchwire::EventManager::instance()->RegisterSignal(
            iter->second,
            iter->first,
            switchwire::EventManager::button_SignalType );
    }

    // register "raw" analog signals with EventManager
    for( GameControllerAnalogSignalMap_type::const_iterator
            iter = m_gameControllerAnalogSignalMap.begin();
            iter != m_gameControllerAnalogSignalMap.end(); ++iter )
    {
        switchwire::EventManager::instance()->RegisterSignal(
            iter->second,
            iter->first,
            switchwire::EventManager::input_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
GameControllerCallbacks::~GameControllerCallbacks()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::SetCharacterState( bool const& enable )
{
    if( enable )
    {
        m_mxGamePadStyle->setStickRate( 0.75 );
    }
    else
    {
        m_mxGamePadStyle->setStickRate( 5.0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
GameControllerCallbacks* GameControllerCallbacks::AsGameController()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::ProcessEvents( ves::open::xml::CommandPtr )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis0Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis0"]->signal( event );

    if( m_exit )
    {
        return;
    }

    UpdateForwardAndUp();

    m_mxGamePadStyle->setButtons( 0 );

    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickX = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    //m_leftStickX *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    if( !m_uiMode )
    {
        bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY,
                       ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

        if( !success )
        {
            return;
        }

        m_success = success;

        m_updateData.signal( m_success );
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis1Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis1"]->signal( event );

    if( m_exit )
    {
        return;
    }

    UpdateForwardAndUp();

    m_mxGamePadStyle->setButtons( 0 );

    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickY = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    //m_leftStickY *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    if( !m_uiMode )
    {
        bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY,
                       ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

        if( !success )
        {
            return;
        }

        m_success = success;

        m_updateData.signal( m_success );
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis2Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis2"]->signal( event );

    if( m_exit )
    {
        return;
    }

    UpdateForwardAndUp();

    m_mxGamePadStyle->setButtons( m_buttons );

    // Right stick: Rotate.
    // Base class angle values are in degrees. By calling
    // normalizeAxisValue, we pass in -1 to 1 degrees.
    // Compensate for rotation as well:
    //  x value around up vector, positive values counterclockwise
    //  y value around right/cross vector, positive values counterclockwise
    //    NOTE .lZ is positive when pulled back. This is the opposite of
    //    the left gamepad stick.
    //x = -normalizeAxisValue( devState.lRz );
    //y = normalizeAxisValue( devState.lZ );
    m_rightStickX = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    if( !m_uiMode )
    {
        m_rightStickX *= -1.0;

        bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY,
                       ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

        if( !success )
        {
            return;
        }

        m_success = success;

        m_updateData.signal( m_success );
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis3Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis3"]->signal( event );

    if( m_exit )
    {
        return;
    }

    UpdateForwardAndUp();

    m_mxGamePadStyle->setButtons( m_buttons );

    // Right stick: Rotate.
    // Base class angle values are in degrees. By calling
    // normalizeAxisValue, we pass in -1 to 1 degrees.
    // Compensate for rotation as well:
    //  x value around up vector, positive values counterclockwise
    //  y value around right/cross vector, positive values counterclockwise
    //    NOTE .lZ is positive when pulled back. This is the opposite of
    //    the left gamepad stick.
    //x = -normalizeAxisValue( devState.lRz );
    //y = normalizeAxisValue( devState.lZ );
    m_rightStickY = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    if( !m_uiMode )
    {
        m_rightStickY *= -1.0;

        bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY,
                       ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

        if( !success )
        {
            return;
        }

        m_success = success;

        m_updateData.signal( m_success );
    }
    else
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis4Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis4"]->signal( event );

    if( m_exit )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnAxis5Event( const float event )
{
    m_gameControllerAnalogSignalMap["GameController.Axis5"]->signal( event );

    if( m_exit )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton0Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button0"]->signal( event );

    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton0Event" << std::endl;
    switch( event )
    {
        case gadget::DigitalState::ON:
        {
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            osg::Vec3d scale = m_viewMatrix.getMoveScale();
            scale -= osg::Vec3d( 0.1, 0.1, 0.1 );
            if( scale[ 0 ] < 0. )
            {
                scale = osg::Vec3d( 1., 1., 1. );
            }
            m_viewMatrix.setMoveScale( scale );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            break;
        }
        default:
            break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton1Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button1"]->signal( event );

    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton1Event" << std::endl;
    switch( event )
    {
        case gadget::DigitalState::ON:
        {
            UpdateForwardAndUp();
            m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button12,
                                         ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            break;
        }
        default:
            break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton2Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button2"]->signal( event );

    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton2Event" << std::endl;
    switch( event )
    {
        case gadget::DigitalState::ON:
        {
            break;
        }
        case gadget::DigitalState::TOGGLE_ON:
        {
            osg::Vec3d scale = m_viewMatrix.getMoveScale();
            scale += osg::Vec3d( 0.1, 0.1, 0.1 );
            m_viewMatrix.setMoveScale( scale );
            break;
        }
        case gadget::DigitalState::TOGGLE_OFF:
        {
            break;
        }
        default:
            break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton3Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button3"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton3Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        UpdateForwardAndUp();
        m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button13,
                                      ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton4Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button4"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton4Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        UpdateForwardAndUp();
        //_mxCore->setPosition( osg::Vec3( 0., 0., 0. ) );
        m_mxGamePadStyle->getMxCore()->level();
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton5Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button5"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton5Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        m_mxGamePadStyle->getMxCore()->reset();
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton6Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button6"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton6Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton7Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button7"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton7Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton8Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button8"]->signal( event );

    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton8Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        m_hideShowUI.signal();
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton9Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button9"]->signal( event );

    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton9Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton10Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button10"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton10Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        if( m_mxGamePadStyle->getMoveMode() == osgwMx::FunctionalMap::MoveModeOrbit )
        {
            m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeOriented );
            m_mxGamePadStyle->setStickRate( 5.0 );
        }
        ///Orbit mode
        else
        {
            m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeOrbit );
            m_mxGamePadStyle->setStickRate( 1.0 );
            m_viewMatrix.lookAtOrbitCenter();
        }
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnButton11Event( gadget::DigitalState::State event )
{
    m_gameControllerButtonSignalMap["GameController.Button11"]->signal( event );

    if( m_exit )
    {
        return;
    }

    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    //std::cout << "GameControllerCallbacks::OnButton11Event" << std::endl;
    switch( event )
    {
    case gadget::DigitalState::ON:
    {
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        if( m_mxGamePadStyle->getRotateMode() == osgwMx::FunctionalMap::RotateModeOrbit )
        {
            m_mxGamePadStyle->setRotateMode( osgwMx::FunctionalMap::RotateModeLocal );
            m_buttons = 0;
        }
        ///Orbit mode
        else
        {
            m_mxGamePadStyle->setRotateMode( osgwMx::FunctionalMap::RotateModeOrbit );
            m_viewMatrix.lookAtOrbitCenter();
            m_buttons = 0;
        }
        break;
    }
    case gadget::DigitalState::TOGGLE_OFF:
    {
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
    ///In quad buffered stereo this call returns a VPW matrix from a centered
    ///view rather than from one of the eye positions.
    osg::Matrixd inverseVPW( m_sceneManager.GetCurrentGLTransformInfo()->GetVPWMatrixOSG() );
    inverseVPW.invert( inverseVPW );
    startPoint = osg::Vec3d( m_currX, m_currY, 0.0f ) * inverseVPW;
    endPoint = osg::Vec3d( m_currX, m_currY, 1.0f ) * inverseVPW;
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::SetRotationMode( std::string rotationMode )
{
    m_navMode = rotationMode;
    if( m_navMode == "User" )
    {
        m_buttons = osgwMx::MxGamePad::Button11;
    }
    else if( m_navMode == "Orbit" )
    {
        m_buttons = osgwMx::MxGamePad::Button10;
        m_viewMatrix.lookAtOrbitCenter();
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::UpdateForwardAndUp()
{
    /*if( (*m_gamecontroller)->isStupefied() )
    {
        return;
    }*/

    gmtl::Matrix44d vrjWandMat = gmtl::convertTo< double >( m_controllerPosition );
    const gmtl::AxisAngled myAxisAngle( osg::DegreesToRadians( double( 90 ) ), 1, 0, 0 );
    const gmtl::Matrix44d myMat = gmtl::make< gmtl::Matrix44d >( myAxisAngle );
    gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
    gmtl::Matrix44d zUpMatrix = gmtl::makeRot< gmtl::Matrix44d >(
                                    gmtl::AxisAngled( gmtl::Math::deg2Rad( -90.0 ), x_axis ) );

    vrjWandMat = myMat * vrjWandMat * zUpMatrix;

    gmtl::Vec3d vjVec;
    vjVec.set( 0.0f, 0.0f, 1.0f );
    gmtl::xform( vjVec, vrjWandMat, vjVec );
    gmtl::normalize( vjVec );
    osg::Vec3d upVec( vjVec.mData[ 0 ], vjVec.mData[ 1 ], vjVec.mData[ 2 ] );
    //m_viewMatrix.setUp( upVec );
    
    vjVec.set( 0.0f, 1.0f, 0.0f );
    gmtl::xform( vjVec, vrjWandMat, vjVec );
    gmtl::normalize( vjVec );
    osg::Vec3d dirVec( vjVec.mData[ 0 ], vjVec.mData[ 1 ], vjVec.mData[ 2 ] );
    //m_viewMatrix.setDir( dirVec );
    
    //This matters in 
    m_viewMatrix.setOriented( upVec, dirVec );
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::ConfigureGameControllerDevices()
{
    m_gameControllerBaseNames.push_back( "VESJoystick0" );
    m_gameControllerBaseNames.push_back( "VESJoystick1" );
    m_gameControllerBaseNames.push_back( "VESJoystick2" );
    m_gameControllerBaseNames.push_back( "VESJoystick3" );
    
    m_lastActiveTime = boost::posix_time::microsec_clock::local_time();

    std::string deviceName;
    for( size_t i = 0; i < m_gameControllerBaseNames.size(); ++i )
    {
        std::string joystickType;
        {
            gadget::DigitalProxyPtr joystick = gadget::DigitalProxy::create( "Joystick" + boost::lexical_cast< std::string >( i ), i );
            joystick->refresh();
            if( !joystick->isStupefied() )
            {
                std::cout << "The game controller is a ";
                joystickType = joystick->getProxiedInputDevice()->getHardwareName();
                std::cout << joystickType << std::endl;
            }
        }
        
        if( !joystickType.empty() )
        {
            std::string xplorerBaseDir;
            vpr::System::getenv( "XPLORER_BASE_DIR", xplorerBaseDir );
            xplorerBaseDir += "/share/vesuite/vrj_configs/";
            jccl::Configuration* configuration = new jccl::Configuration();
            if( joystickType == "Wireless 360 Controller" )
            {
                configuration->load( xplorerBaseDir + "xbox_360_js" + boost::lexical_cast< std::string >( i ) + ".jconf" );
            }
            else if( (joystickType == "Logitech Cordless RumblePad 2") || (joystickType == "Logitech Logitech Cordless RumblePad 2") )
            {
                configuration->load( xplorerBaseDir + "rumble_pad_js" + boost::lexical_cast< std::string >( i ) + ".jconf" );
            }
            else if( joystickType == "Controller (Xbox 360 Wireless Receiver for Windows)" )
            {
                configuration->load( xplorerBaseDir + "xbox_360_windows_js" + boost::lexical_cast< std::string >( i ) + ".jconf" );
            }
            else
            {
                std::cout << "This " << joystickType << " game controller is not supported. We will use" << std::endl
                    << "the generic joystick config file generic_js.jconf." << std::endl;
                configuration->load( xplorerBaseDir + "generic_js" + boost::lexical_cast< std::string >( i ) + ".jconf" );
            }
            jccl::ConfigManager::instance()->addConfigurationAdditions( configuration );
            delete configuration;

            deviceName = m_gameControllerBaseNames[ i ];
            m_gamControllerEvents[ m_gameControllerBaseNames[ i ] ] = new GameController( i );
            m_gamControllerEvents[ m_gameControllerBaseNames[ i ] ]->InitInterfaces( deviceName );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::GrabControlState( unsigned int const& controllerMask )
{
    if( m_controlledState & ClosedControlState )
    {
        if( controllerMask == m_activeController )
        {
            //Reset the timer
            m_lastActiveTime = boost::posix_time::microsec_clock::local_time();
        }
        return;
    }

    if( m_controlledState & OpenControlState )
    {
        m_gamControllerEvents[ m_gameControllerBaseNames[ controllerMask ]  ]->
            ConnectInterfaces( this );
        m_activeController = controllerMask;
        m_controlledState = ClosedControlState;
        //Reset the timer
        m_lastActiveTime = boost::posix_time::microsec_clock::local_time();
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::CheckControlState()
{
    if( m_controlledState & OpenControlState )
    {
        return;
    }

    ///If timer is greater than time out
    boost::posix_time::ptime current_time( boost::posix_time::microsec_clock::local_time() );
    boost::posix_time::time_duration diff = current_time - m_lastActiveTime;
    double frameTime = diff.total_milliseconds() * 0.001;
    //std::cout << frameTime << std::endl;
    if( frameTime > 3.0 )
    {
        m_controlledState = OpenControlState;
        m_gamControllerEvents[ m_gameControllerBaseNames[ m_activeController ]  ]->
            DisconnectInterfaces( this );
    }
}
////////////////////////////////////////////////////////////////////////////////
void GameControllerCallbacks::OnPositionEvent( gmtl::Matrix44f mat )
{
    m_controllerPosition = mat;
}
////////////////////////////////////////////////////////////////////////////////
