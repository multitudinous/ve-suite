/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <ves/open/xml/model/Model.h>

#include <osgwMx/MxEventHandler.h>
#include <ves/xplorer/device/MxInputAdapterGadgeteerGamePad.h>
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
    m_rightStickY( 0 )
{
    m_mxGamePadStyle->setStickRate( 10.0 );
    m_mxGamePadStyle->setStickDeadZone( 0.05f );

    // Set some MxCore defaults:
    osgwMx::MxCore* mxCore = m_mxGamePadStyle->getMxCore();
    mxCore->setInitialValues( 
        osg::Vec3d( 0., 1., 0. ), osg::Vec3d( 0., 0., -1. ), osg::Vec3d( 0., 0., 0. ) );
    mxCore->reset();
    
    // Connect to Juggler's new event handling interface
    //Left stick - X
    m_analogAxis0EventInterface.init("VJAxis0");
    //m_analogAxis0EventInterface.addCallback(boost::bind(&GameController::OnAxis0Event, this, _1));
    m_analogAxis0EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind(&GameController::OnAxis0Event, this, _1) );
    //m_analogAxis0EventInterface.addCallback<raw_analog_event_tag>(boost::bind(&GameController::OnAxis0Event, this, _1));
    
    //Left stick - Y
    m_analogAxis1EventInterface.init("VJAxis1");
    m_analogAxis1EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis1Event, this, _1));

    //Right stick - X
    m_analogAxis2EventInterface.init("VJAxis2");
    m_analogAxis2EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis2Event, this, _1));

    //Right stick - X
    m_analogAxis3EventInterface.init("VJAxis3");
    m_analogAxis3EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis3Event, this, _1));

    m_analogAxis4EventInterface.init("VJAxis4");
    m_analogAxis4EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis4Event, this, _1));

    m_analogAxis5EventInterface.init("VJAxis5");
    m_analogAxis5EventInterface.addCallback<gadget::event::normalized_analog_event_tag>(boost::bind(&GameController::OnAxis5Event, this, _1));
    
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
    
    CONNECTSIGNALS_1( "%Exit", void( bool const& ),
                     &GameController::Exit,
                     m_connections, any_SignalType, normal_Priority );  
    */
    //Setup the ability to catch shutdowns
    //m_signalHandler = boost::bind(&GameController::HandleSignal, this, _1);
    //vrj::Kernel::instance()->
    //    addHandlerPreCallback( boost::bind(&GameController::HandleSignal, this, _1) );
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
void GameController::Exit( bool const& exit )
{
    m_exit = exit;
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

    m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button2 );//osgwMx::MxGamePad::Button8 );

    m_mxGamePadStyle->getMxCore()->setByMatrix( 
        osg::Matrix( DeviceHandler::instance()->GetActiveDCS()->GetMat().getData() ) );
    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickX = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    m_leftStickX *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }
    
    gmtl::Matrix44d newTransform;
    newTransform.set( m_mxGamePadStyle->getMxCore()->getMatrix().ptr() );
    
    //Set the activeDCS w/ new transform
    DeviceHandler::instance()->GetActiveDCS()->SetMat( newTransform );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis1Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button2 );//osgwMx::MxGamePad::Button8 );
    
    m_mxGamePadStyle->getMxCore()->setByMatrix( 
        osg::Matrix( DeviceHandler::instance()->GetActiveDCS()->GetMat().getData() ) );
    
    // Left stick: Move.
    // Normalize values to range -1.0 to 1.0.
    // These are units to move in world coordinates per event or per frame.
    m_leftStickY = ( ( event - 0.0f ) / 0.5f ) - 1.f;
    m_leftStickY *= -1.0;
    //float y = normalizeAxisValue( devState.lY );
    bool success = m_mxGamePadStyle->setLeftStick( m_leftStickX, m_leftStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }
        
    gmtl::Matrix44d newTransform;
    newTransform.set( m_mxGamePadStyle->getMxCore()->getMatrix().ptr() );

    //Set the activeDCS w/ new transform
    DeviceHandler::instance()->GetActiveDCS()->SetMat( newTransform );

}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis2Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button11 );

    m_mxGamePadStyle->getMxCore()->setByMatrix( 
        osg::Matrix( DeviceHandler::instance()->GetActiveDCS()->GetMat().getData() ) );

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

    bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );
    
    if( !success )
    {
        return;
    }
    
    gmtl::Matrix44d newTransform;    
    newTransform.set( m_mxGamePadStyle->getMxCore()->getMatrix().ptr() );
    //Set the activeDCS w/ new transform
    DeviceHandler::instance()->GetActiveDCS()->SetMat( newTransform );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis3Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    m_mxGamePadStyle->setButtons( osgwMx::MxGamePad::Button11 );
    
    m_mxGamePadStyle->getMxCore()->setByMatrix( 
        osg::Matrix( DeviceHandler::instance()->GetActiveDCS()->GetMat().getData() ) );

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
    
    bool success = m_mxGamePadStyle->setRightStick( m_rightStickX, m_rightStickY, 
        ves::xplorer::scenegraph::SceneManager::instance()->GetDeltaFrameTime() );

    if( !success )
    {
        return;
    }

    gmtl::Matrix44d newTransform;    
    newTransform.set( m_mxGamePadStyle->getMxCore()->getMatrix().ptr() );
    //Set the activeDCS w/ new transform
    DeviceHandler::instance()->GetActiveDCS()->SetMat( newTransform );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis4Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    std::cout << " Analog device input OnAxis4Event " 
        << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis5Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    std::cout << " Analog device input OnAxis5Event " 
        << event << std::endl << std::flush;
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
    
    //std::cout << m_currX << " " << m_currY << std::endl << std::flush;
    //std::cout << "startPoint: " << startPoint << std::endl << std::flush;
    //std::cout << "endPoint: " << endPoint << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
