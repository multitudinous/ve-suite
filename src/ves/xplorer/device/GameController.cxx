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
#include <ves/xplorer/device/GameController.h>

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

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/open/xml/model/Model.h>

#include <osgwMx/MxGamePad.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- vrJuggler Includes --- //
#include <vrj/vrjParam.h>

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
GameController::GameController()
    :
    Device( KEYBOARD_MOUSE ),
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
    m_success( false )
{
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
    m_mxGamePadStyle->setStickRate( 10.0 );
    m_mxGamePadStyle->setStickDeadZone( 0.05f );
    m_mxGamePadStyle->setMxCore( &m_viewMatrix );
    //MoveModeLiteral - moves in opengl / eye space
    //MoveModeWorld - moves in rotate object space
    //MoveModeLocal - moves in global coordinates
    m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeLocal );
    
    // Connect to Juggler's new event handling interface
    //Left stick - X
    m_analogAxis0EventInterface.init("VJAxis0");
    m_analogAxis0EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind(&GameController::OnAxis0Event, this, _1) );
    //m_analogAxis0EventInterface.addCallback<raw_analog_event_tag>(boost::bind(&GameController::OnAxis0Event, this, _1));
    
    //Left stick - Y
    m_analogAxis1EventInterface.init("VJAxis1");
    m_analogAxis1EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis1Event, this, _1));

    //Right stick - X
    m_analogAxis2EventInterface.init("VJAxis2");
    m_analogAxis2EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis2Event, this, _1));

    //Right stick - Y
    m_analogAxis3EventInterface.init("VJAxis3");
    m_analogAxis3EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis3Event, this, _1));
    
    //All the buttons
    m_button0EventInterface.init( "Joystick0_d0" );
    m_button0EventInterface.addCallback(boost::bind(&GameController::OnButton0Event, this, _1));

    m_button1EventInterface.init( "Joystick0_d1" );
    m_button1EventInterface.addCallback(boost::bind(&GameController::OnAxis5Event, this, _1));

    m_button2EventInterface.init( "Joystick0_d2" );
    m_button2EventInterface.addCallback(boost::bind(&GameController::OnButton2Event, this, _1));

    m_button3EventInterface.init( "Joystick0_d3" );
    m_button3EventInterface.addCallback(boost::bind(&GameController::OnAxis4Event, this, _1));

    m_button4EventInterface.init( "Joystick0_d4" );
    m_button4EventInterface.addCallback(boost::bind(&GameController::OnButton4Event, this, _1));

    m_button5EventInterface.init( "Joystick0_d5" );
    m_button5EventInterface.addCallback(boost::bind(&GameController::OnButton5Event, this, _1));

    m_button6EventInterface.init( "Joystick0_d6" );
    m_button6EventInterface.addCallback(boost::bind(&GameController::OnButton6Event, this, _1));

    m_button7EventInterface.init( "Joystick0_d7" );
    m_button7EventInterface.addCallback(boost::bind(&GameController::OnButton7Event, this, _1));

    m_button10EventInterface.init( "Joystick0_d10" );
    m_button10EventInterface.addCallback(boost::bind(&GameController::OnButton10Event, this, _1));

    m_button11EventInterface.init( "Joystick0_d11" );
    m_button11EventInterface.addCallback(boost::bind(&GameController::OnButton11Event, this, _1));

    /*eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    evm->RegisterSignal(
        new SignalWrapper< MouseMoveSignal_type >( &m_mouseMove ),
        "GameController.MouseMove", eventmanager::EventManager::mouse_SignalType );

    evm->RegisterSignal(
        new SignalWrapper< MouseDoubleClickSignal_type >( &m_mouseDoubleClick ),
        "GameController.DoubleClick", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
        new SignalWrapper< ScrollSignal_type >( &m_scroll ),
        "GameController.Scroll", eventmanager::EventManager::input_SignalType );

    evm->RegisterSignal(
        new SignalWrapper< StartEndPointSignal_type >( &m_startEndPointSignal ),
        "GameController.StartEndPoint", eventmanager::EventManager::unspecified_SignalType );
    
    RegisterButtonSignals();
    RegisterKeySignals();
    */    
    CONNECTSIGNALS_1( "%NavigationRotationMode", void( std::string ),
                     &GameController::SetRotationMode,
                     m_connections, any_SignalType, normal_Priority );  

    // Register signal(s) with EventManager
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::BoolSignal_type >( &m_updateData ),
        "GameController.UpdateData");

    //m_hideShowUI =
    //    reinterpret_cast< eventmanager::SignalWrapper< ves::util::VoidSignal_type >* >
    //    ( eventmanager::EventFactory::instance()->GetSignal( "EventMapper.HideShowUI" ) )
    //    ->mSignal;
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::VoidSignal_type >( &m_hideShowUI ),
        "GameController.HideShowUI");
}
////////////////////////////////////////////////////////////////////////////////
GameController::~GameController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
GameController* GameController::AsGameController()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::ProcessEvents( ves::open::xml::CommandPtr )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis0Event( const float event )
{
    if( m_exit )
    {
        return;
    }

    m_mxGamePadStyle->setButtons( 0 );

    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickX = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    //m_leftStickX *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }

    m_success = success;

    m_updateData( m_success );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis1Event( const float event )
{
    if( m_exit )
    {
        return;
    }

    m_mxGamePadStyle->setButtons( 0 );
    
    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickY = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    //m_leftStickY *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }
    
    m_success = success;
    
    m_updateData( m_success );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis2Event( const float event )
{
    if( m_exit )
    {
        return;
    }
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
    m_rightStickX *= -1.0;

    bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }
    
    m_success = success;
    
    m_updateData( m_success );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis3Event( const float event )
{
    if( m_exit )
    {
        return;
    }
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
    m_rightStickY *= -1.0;

    bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }
    
    m_success = success;
    
    m_updateData( m_success );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis4Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {   
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
void GameController::OnAxis5Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {        
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
void GameController::OnButton0Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {   
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        osg::Vec3d scale = m_viewMatrix.getMoveScale();
        scale -= osg::Vec3d( 1., 1., 1. );
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
void GameController::OnButton2Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {   
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        osg::Vec3d scale = m_viewMatrix.getMoveScale();
        scale += osg::Vec3d( 1., 1., 1. );
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
void GameController::OnButton4Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {   
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
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
void GameController::OnButton5Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
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
void GameController::OnButton6Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }

    switch(event) 
    {
    case gadget::DigitalState::ON:
    {        
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        m_hideShowUI();
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
void GameController::OnButton7Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
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
void GameController::OnButton10Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
    {
    case gadget::DigitalState::ON:
    {        
        break;
    }
    case gadget::DigitalState::TOGGLE_ON:
    {
        if( m_mxGamePadStyle->getMoveMode() == osgwMx::FunctionalMap::MoveModeOrbit )
        {
            m_mxGamePadStyle->setMoveMode( osgwMx::FunctionalMap::MoveModeLocal );
            m_mxGamePadStyle->setStickRate( 10.0 );
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
void GameController::OnButton11Event( gadget::DigitalState::State event )
{
    if( m_exit )
    {
        return;
    }
    
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    switch(event) 
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
void GameController::SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
    ///In quad buffered stereo this call returns a VPW matrix from a centered
    ///view rather than from one of the eye positions.
    osg::Matrixd inverseVPW( m_sceneManager.GetCurrentGLTransformInfo()->GetVPWMatrixOSG() );
    inverseVPW.invert( inverseVPW );
    startPoint = osg::Vec3d( m_currX, m_currY, 0.0f ) * inverseVPW;
    endPoint = osg::Vec3d( m_currX, m_currY, 1.0f ) * inverseVPW;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::SetRotationMode( std::string rotationMode )
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
