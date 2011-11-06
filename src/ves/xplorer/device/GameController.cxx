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
// #include <ves/open/xml/DataValuePair.h>
// #include <ves/open/xml/Command.h>
// #include <ves/xplorer/command/CommandManager.h>

#include <osgwMx/MxEventHandler.h>
#include <ves/xplorer/device/MxInputAdapterGadgeteerGamePad.h>
#include <osgwMx/MxMovementStyleGamePad.h>

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
    m_gadgetInputAdapter( new osgwMx::MxInputAdapterGadgeteerGamePad() ),
    m_mxGamePadStyle( new osgwMx::MxMovementStyleGamePad( m_gadgetInputAdapter.get() ) )
{
    // Connect to Juggler's new event handling interface
    m_analogAxis0EventInterface.init("VJAxis0");
    m_analogAxis0EventInterface.addCallback(boost::bind(&GameController::OnAxis0Event, this, _1));

    m_analogAxis1EventInterface.init("VJAxis1");
    m_analogAxis1EventInterface.addCallback(boost::bind(&GameController::OnAxis1Event, this, _1));

    m_analogAxis2EventInterface.init("VJAxis2");
    m_analogAxis2EventInterface.addCallback(boost::bind(&GameController::OnAxis2Event, this, _1));

    m_analogAxis3EventInterface.init("VJAxis3");
    m_analogAxis3EventInterface.addCallback(boost::bind(&GameController::OnAxis3Event, this, _1));

    m_analogAxis4EventInterface.init("VJAxis4");
    m_analogAxis4EventInterface.addCallback(boost::bind(&GameController::OnAxis4Event, this, _1));

    m_analogAxis5EventInterface.init("VJAxis5");
    m_analogAxis5EventInterface.addCallback(boost::bind(&GameController::OnAxis5Event, this, _1));
    
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

    m_gadgetInputAdapter->clear();
    m_gadgetInputAdapter->ExtractAxis( MOVE_X_AXIS_IDX, event );
    //m_mxGamePadStyle->matrixTransform( 
    //    ves::xplorer::scenegraph::SceneManager::instance()->GetFrameStamp()->getSimulationTime() );
    osg::Matrixd navMatrix = m_mxGamePadStyle->getMxCore()->getMatrix();
    //std::cout << " Analog device input " << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis1Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    //std::cout << " Analog device input " << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis2Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    //std::cout << " Analog device input " << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis3Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    //std::cout << " Analog device input " << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis4Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    //std::cout << " Analog device input " << event << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis5Event( const float event )
{
    if( m_exit )
    {
        return;
    }
    
    //std::cout << " Analog device input " << event << std::endl << std::flush;
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
