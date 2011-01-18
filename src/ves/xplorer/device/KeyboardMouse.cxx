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
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/behavior/Selection.h>
#include <ves/xplorer/behavior/Navigation.h>

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

#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/PointLineSegmentIntersector.h>

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



// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- vrJuggler Includes --- //
#include <vrj/vrjParam.h>

#include <vrj/Draw/OpenGL/Window.h>

#if defined( VPR_OS_Darwin )
#include <vrj/Draw/OpenGL/WindowCocoa.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowCocoa.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaCocoa.h>
#elif defined( VPR_OS_Windows )
#include <vrj/Draw/OpenGL/WindowWin32.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowWin32.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaWin32.h>
#elif defined( VPR_OS_Linux )
#include <vrj/Draw/OpenGL/WindowXWin.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowXWin.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaXWin.h>
#endif

#include <gadget/Event/KeyboardMouse/KeyEvent.h>
#include <gadget/Event/KeyboardMouse/MouseEvent.h>

#include <gadget/Devices/KeyboardMouseDevice/InputArea.h>

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

// --- osgBullet Includes --- //
#include <osgwTools/AbsoluteModelTransform.h>
#include <osgbBullet/RefRigidBody.h>

// --- STL Includes --- //
#include <iostream>
#include <cmath>

using namespace ves::xplorer::device;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
    :
    Device( KEYBOARD_MOUSE ),
    m_navigationSlot( new ves::xplorer::behavior::Navigation() ),
    m_selectionSlot( new ves::xplorer::behavior::Selection() ),
    m_mouseInsideUI( true )
{
    //mHead.init( "VJHead" );

    // Connect to Juggler's new event handling interface
    m_mouseDoubleClickEventInterface.setClickTime(300);
    m_mouseDoubleClickEventInterface.init("VJKeyboard");
    m_mouseDoubleClickEventInterface.addCallback(boost::bind(&KeyboardMouse::onMouseDoubleClick, this, _1));

    m_keyboardMouseEventInterface.init("VJKeyboard");
    m_keyboardMouseEventInterface.addCallback(boost::bind(&KeyboardMouse::onKeyboardMouseEvent, this, _1));

    SetupButtonSignalMap();
    SetupKeySignalMap();

    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    evm->RegisterSignal(
            new SignalWrapper< MouseMoveSignal_type >( &m_mouseMove ),
            "KeyboardMouse.MouseMove", eventmanager::EventManager::mouse_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< MouseDoubleClickSignal_type >( &m_mouseDoubleClick ),
            "KeyboardMouse.DoubleClick", eventmanager::EventManager::button_SignalType );

    RegisterButtonSignals();
    RegisterKeySignals();
    
    CONNECTSIGNAL_1( "UIManager.EnterLeaveUI", void( bool ), &KeyboardMouse::UIEnterLeave,
                    m_connections, highest_Priority );
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::~KeyboardMouse()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* KeyboardMouse::AsKeyboardMouse()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UIEnterLeave( bool insideUI )
{
    m_mouseInsideUI = insideUI;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessEvents( ves::open::xml::CommandPtr command )
{

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::onKeyboardMouseEvent(gadget::EventPtr event)
{
    const gadget::EventType eventType = event->type();
    
    //Get the current display from the input area
    gadget::InputArea& inputArea = event->getSource();
    vrj::DisplayPtr currentDisplay = GetCurrentDisplay( &inputArea );
    
    switch( eventType )
    {
    case gadget::KeyPressEvent:
    {
        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, true ) )
        {
            return;
        }
        /*vprDEBUG( vesDBG, 4 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::KeyPressEvent"
            << std::endl << vprDEBUG_FLUSH;*/

        const gadget::KeyEventPtr keyEvt =
            boost::static_pointer_cast< gadget::KeyEvent >( event );

        // Emit the keypress signal associated with this particular key
        KeyPressSignalMapType::iterator itr = mKeyPressSignalMap.find( keyEvt->getKey() );
        if( itr != mKeyPressSignalMap.end() )
        {
            (*(itr->second))( keyEvt->getKey(), keyEvt->getModifierMask(),
                              //For use when unicode works in VR Juggler
                              //keyEvt->getKeyUnicode() );
                              keyEvt->getKeyChar() );
        }
        else
        {
            vprDEBUG( vesDBG, 2 )
                << "|\tKeyboardMouse::onKeyboardMouseEvent::Unknown key in KeyPress event"
                << std::endl << vprDEBUG_FLUSH;
        }

        break;
    }
    case gadget::KeyReleaseEvent:
    {
        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, true ) )
        {
            return;
        }
        /*vprDEBUG( vesDBG, 4 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::KeyReleaseEvent"
            << std::endl << vprDEBUG_FLUSH;*/

        const gadget::KeyEventPtr keyEvt =
            boost::static_pointer_cast< gadget::KeyEvent >( event );

        // Emit the keypress signal associated with this particular key
        KeyReleaseSignalMapType::iterator itr = mKeyReleaseSignalMap.find( keyEvt->getKey() );
        if( itr != mKeyReleaseSignalMap.end() )
        {
            (*(itr->second))( keyEvt->getKey(), keyEvt->getModifierMask(),
                             //For use when unicode works in VR Juggler
                             //keyEvt->getKeyUnicode() );
                             keyEvt->getKeyChar() );
        }
        else
        {
            vprDEBUG( vesDBG, 2 )
                << "|\tKeyboardMouse::onKeyboardMouseEvent::Unknown key in KeyRelease event"
                << std::endl << vprDEBUG_FLUSH;
        }

        break;
    }
    case gadget::MouseButtonPressEvent:
    {
        const gadget::MouseEventPtr mouseEvt =
            boost::static_pointer_cast< gadget::MouseEvent >( event );
        
        m_currX = mouseEvt->getX();
        m_currY = mouseEvt->getY();
        
        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }        
        
        /*vprDEBUG( vesDBG, 4 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::MouseButtonPressEvent "
            << mouseEvt->getButton() << ", " << mouseEvt->getX()
            << ", " << mouseEvt->getY() << ", " << mouseEvt->getState()
            << std::endl << vprDEBUG_FLUSH;*/

        ButtonPressSignalMapType::iterator itr = mButtonPressSignalMap.find( mouseEvt->getButton() );
        if( itr != mButtonPressSignalMap.end() )
        {
            (*(itr->second))( mouseEvt->getButton(), mouseEvt->getX(), mouseEvt->getY(), mouseEvt->getState() );
        }

        break;
    }
    case gadget::MouseButtonReleaseEvent:
    {
        /*vprDEBUG( vesDBG, 4 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::MouseButtonReleaseEvent"
            << std::endl << vprDEBUG_FLUSH;*/
        //Set the current GLTransfromInfo from the event

        const gadget::MouseEventPtr mouseEvt =
            boost::static_pointer_cast< gadget::MouseEvent >( event );

        m_currX = mouseEvt->getX();
        m_currY = mouseEvt->getY();

        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }        
        
        ButtonReleaseSignalMapType::iterator itr = mButtonReleaseSignalMap.find( mouseEvt->getButton() );
        if( itr != mButtonReleaseSignalMap.end() )
        {
            (*(itr->second))( mouseEvt->getButton(), mouseEvt->getX(), mouseEvt->getY(), mouseEvt->getState() );
        }

        break;
    }
    case gadget::MouseMoveEvent:
    {
        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }

        const gadget::MouseEventPtr mouseEvt =
            boost::static_pointer_cast< gadget::MouseEvent >( event );

        /*vprDEBUG( vesDBG, 2 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::MouseMoveEvent"
            << mouseEvt->getButton() << " " << mouseEvt->getX() << ", " << mouseEvt->getY()
            << ", 0, " << mouseEvt->getState()
            << std::endl << vprDEBUG_FLUSH;
        // x, y, z, state (modifier mask OR'd with button mask)*/
        //int buttonMask = mouseEvt->getState();
        //bool test = buttonMask&gadget::BUTTON1_MASK;
        m_mouseMove( mouseEvt->getX(), mouseEvt->getY(), 0, mouseEvt->getState() );

        break;
    }
    default:
    {
        std::cout << "KeyboardMouse event not implemented." << std::endl;
    }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::onMouseDoubleClick( gadget::EventPtr event )
{
    const gadget::MouseEventPtr mouseEvt =
        boost::static_pointer_cast< gadget::MouseEvent >( event );

    /*vprDEBUG( vesDBG, 2 )
        << "|\tKeyboardMouse::onMouseDoubleClick"
        << mouseEvt->getButton() << ", " << mouseEvt->getX()
        << ", " << mouseEvt->getY() << ", " << mouseEvt->getState()
        << std::endl << vprDEBUG_FLUSH;*/

    m_mouseDoubleClick( mouseEvt->getButton(), mouseEvt->getX(),
                       mouseEvt->getY(), 0, mouseEvt->getState() );

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetupButtonSignalMap()
{
    mButtonPressSignalMap[ gadget::MBUTTON1 ] = &mButtonPress_1;
    mButtonReleaseSignalMap[ gadget::MBUTTON1 ] = &mButtonRelease_1;

    mButtonPressSignalMap[ gadget::MBUTTON2 ] = &mButtonPress_2;
    mButtonReleaseSignalMap[ gadget::MBUTTON2 ] = &mButtonRelease_2;

    mButtonPressSignalMap[ gadget::MBUTTON3 ] = &mButtonPress_3;
    mButtonReleaseSignalMap[ gadget::MBUTTON3 ] = &mButtonRelease_3;

    mButtonPressSignalMap[ gadget::MBUTTON4 ] = &mButtonPress_4;
    mButtonReleaseSignalMap[ gadget::MBUTTON4 ] = &mButtonRelease_4;

    mButtonPressSignalMap[ gadget::MBUTTON5 ] = &mButtonPress_5;
    mButtonReleaseSignalMap[ gadget::MBUTTON5 ] = &mButtonRelease_5;

    mButtonPressSignalMap[ gadget::MBUTTON6 ] = &mButtonPress_6;
    mButtonReleaseSignalMap[ gadget::MBUTTON6 ] = &mButtonRelease_6;

    mButtonPressSignalMap[ gadget::MBUTTON7 ] = &mButtonPress_7;
    mButtonReleaseSignalMap[ gadget::MBUTTON7 ] = &mButtonRelease_7;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RegisterButtonSignals()
{
    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_1 ),
            "KeyboardMouse.ButtonPress1", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_1 ),
            "KeyboardMouse.ButtonRelease1", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_2 ),
            "KeyboardMouse.ButtonPress2", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_2 ),
            "KeyboardMouse.ButtonRelease2", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_3 ),
            "KeyboardMouse.ButtonPress3", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_3 ),
            "KeyboardMouse.ButtonRelease3", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_4 ),
            "KeyboardMouse.ButtonPress4", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_4 ),
            "KeyboardMouse.ButtonRelease4", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_5 ),
            "KeyboardMouse.ButtonPress5", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_5 ),
            "KeyboardMouse.ButtonRelease5", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_6 ),
            "KeyboardMouse.ButtonPress6", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_6 ),
            "KeyboardMouse.ButtonRelease6", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( &mButtonPress_7 ),
            "KeyboardMouse.ButtonPress7", eventmanager::EventManager::button_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( &mButtonRelease_7 ),
            "KeyboardMouse.ButtonRelease7", eventmanager::EventManager::button_SignalType );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetupKeySignalMap()
{
    mKeyPressSignalMap[gadget::KEY_NONE] = &mKeyPress_KEY_NONE;
    mKeyReleaseSignalMap[gadget::KEY_NONE] = &mKeyRelease_KEY_NONE;

    mKeyPressSignalMap[gadget::KEY_UP] = &mKeyPress_KEY_UP;
    mKeyReleaseSignalMap[gadget::KEY_UP] = &mKeyRelease_KEY_UP;

    mKeyPressSignalMap[gadget::KEY_DOWN] = &mKeyPress_KEY_DOWN;
    mKeyReleaseSignalMap[gadget::KEY_DOWN] = &mKeyRelease_KEY_DOWN;

    mKeyPressSignalMap[gadget::KEY_LEFT] = &mKeyPress_KEY_LEFT;
    mKeyReleaseSignalMap[gadget::KEY_LEFT] = &mKeyRelease_KEY_LEFT;

    mKeyPressSignalMap[gadget::KEY_RIGHT] = &mKeyPress_KEY_RIGHT;
    mKeyReleaseSignalMap[gadget::KEY_RIGHT] = &mKeyRelease_KEY_RIGHT;

    mKeyPressSignalMap[gadget::KEY_SHIFT] = &mKeyPress_KEY_SHIFT;
    mKeyReleaseSignalMap[gadget::KEY_SHIFT] = &mKeyRelease_KEY_SHIFT;

    mKeyPressSignalMap[gadget::KEY_CTRL] = &mKeyPress_KEY_CTRL;
    mKeyReleaseSignalMap[gadget::KEY_CTRL] = &mKeyRelease_KEY_CTRL;

    mKeyPressSignalMap[gadget::KEY_ALT] = &mKeyPress_KEY_ALT;
    mKeyReleaseSignalMap[gadget::KEY_ALT] = &mKeyRelease_KEY_ALT;

    mKeyPressSignalMap[gadget::KEY_COMMAND] = &mKeyPress_KEY_COMMAND;
    mKeyReleaseSignalMap[gadget::KEY_COMMAND] = &mKeyRelease_KEY_COMMAND;

    mKeyPressSignalMap[gadget::KEY_1] = &mKeyPress_KEY_1;
    mKeyReleaseSignalMap[gadget::KEY_1] = &mKeyRelease_KEY_1;

    mKeyPressSignalMap[gadget::KEY_2] = &mKeyPress_KEY_2;
    mKeyReleaseSignalMap[gadget::KEY_2] = &mKeyRelease_KEY_2;

    mKeyPressSignalMap[gadget::KEY_3] = &mKeyPress_KEY_3;
    mKeyReleaseSignalMap[gadget::KEY_3] = &mKeyRelease_KEY_3;

    mKeyPressSignalMap[gadget::KEY_4] = &mKeyPress_KEY_4;
    mKeyReleaseSignalMap[gadget::KEY_4] = &mKeyRelease_KEY_4;

    mKeyPressSignalMap[gadget::KEY_5] = &mKeyPress_KEY_5;
    mKeyReleaseSignalMap[gadget::KEY_5] = &mKeyRelease_KEY_5;

    mKeyPressSignalMap[gadget::KEY_6] = &mKeyPress_KEY_6;
    mKeyReleaseSignalMap[gadget::KEY_6] = &mKeyRelease_KEY_6;

    mKeyPressSignalMap[gadget::KEY_7] = &mKeyPress_KEY_7;
    mKeyReleaseSignalMap[gadget::KEY_7] = &mKeyRelease_KEY_7;

    mKeyPressSignalMap[gadget::KEY_8] = &mKeyPress_KEY_8;
    mKeyReleaseSignalMap[gadget::KEY_8] = &mKeyRelease_KEY_8;

    mKeyPressSignalMap[gadget::KEY_9] = &mKeyPress_KEY_9;
    mKeyReleaseSignalMap[gadget::KEY_9] = &mKeyRelease_KEY_9;

    mKeyPressSignalMap[gadget::KEY_0] = &mKeyPress_KEY_0;
    mKeyReleaseSignalMap[gadget::KEY_0] = &mKeyRelease_KEY_0;

    mKeyPressSignalMap[gadget::KEY_A] = &mKeyPress_KEY_A;
    mKeyReleaseSignalMap[gadget::KEY_A] = &mKeyRelease_KEY_A;

    mKeyPressSignalMap[gadget::KEY_B] = &mKeyPress_KEY_B;
    mKeyReleaseSignalMap[gadget::KEY_B] = &mKeyRelease_KEY_B;

    mKeyPressSignalMap[gadget::KEY_C] = &mKeyPress_KEY_C;
    mKeyReleaseSignalMap[gadget::KEY_C] = &mKeyRelease_KEY_C;

    mKeyPressSignalMap[gadget::KEY_D] = &mKeyPress_KEY_D;
    mKeyReleaseSignalMap[gadget::KEY_D] = &mKeyRelease_KEY_D;

    mKeyPressSignalMap[gadget::KEY_E] = &mKeyPress_KEY_E;
    mKeyReleaseSignalMap[gadget::KEY_E] = &mKeyRelease_KEY_E;

    mKeyPressSignalMap[gadget::KEY_F] = &mKeyPress_KEY_F;
    mKeyReleaseSignalMap[gadget::KEY_F] = &mKeyRelease_KEY_F;

    mKeyPressSignalMap[gadget::KEY_G] = &mKeyPress_KEY_G;
    mKeyReleaseSignalMap[gadget::KEY_G] = &mKeyRelease_KEY_G;

    mKeyPressSignalMap[gadget::KEY_H] = &mKeyPress_KEY_H;
    mKeyReleaseSignalMap[gadget::KEY_H] = &mKeyRelease_KEY_H;

    mKeyPressSignalMap[gadget::KEY_I] = &mKeyPress_KEY_I;
    mKeyReleaseSignalMap[gadget::KEY_I] = &mKeyRelease_KEY_I;

    mKeyPressSignalMap[gadget::KEY_J] = &mKeyPress_KEY_J;
    mKeyReleaseSignalMap[gadget::KEY_J] = &mKeyRelease_KEY_J;

    mKeyPressSignalMap[gadget::KEY_K] = &mKeyPress_KEY_K;
    mKeyReleaseSignalMap[gadget::KEY_K] = &mKeyRelease_KEY_K;

    mKeyPressSignalMap[gadget::KEY_L] = &mKeyPress_KEY_L;
    mKeyReleaseSignalMap[gadget::KEY_L] = &mKeyRelease_KEY_L;

    mKeyPressSignalMap[gadget::KEY_M] = &mKeyPress_KEY_M;
    mKeyReleaseSignalMap[gadget::KEY_M] = &mKeyRelease_KEY_M;

    mKeyPressSignalMap[gadget::KEY_N] = &mKeyPress_KEY_N;
    mKeyReleaseSignalMap[gadget::KEY_N] = &mKeyRelease_KEY_N;

    mKeyPressSignalMap[gadget::KEY_O] = &mKeyPress_KEY_O;
    mKeyReleaseSignalMap[gadget::KEY_O] = &mKeyRelease_KEY_O;

    mKeyPressSignalMap[gadget::KEY_P] = &mKeyPress_KEY_P;
    mKeyReleaseSignalMap[gadget::KEY_P] = &mKeyRelease_KEY_P;

    mKeyPressSignalMap[gadget::KEY_Q] = &mKeyPress_KEY_Q;
    mKeyReleaseSignalMap[gadget::KEY_Q] = &mKeyRelease_KEY_Q;

    mKeyPressSignalMap[gadget::KEY_R] = &mKeyPress_KEY_R;
    mKeyReleaseSignalMap[gadget::KEY_R] = &mKeyRelease_KEY_R;

    mKeyPressSignalMap[gadget::KEY_S] = &mKeyPress_KEY_S;
    mKeyReleaseSignalMap[gadget::KEY_S] = &mKeyRelease_KEY_S;

    mKeyPressSignalMap[gadget::KEY_T] = &mKeyPress_KEY_T;
    mKeyReleaseSignalMap[gadget::KEY_T] = &mKeyRelease_KEY_T;

    mKeyPressSignalMap[gadget::KEY_U] = &mKeyPress_KEY_U;
    mKeyReleaseSignalMap[gadget::KEY_U] = &mKeyRelease_KEY_U;

    mKeyPressSignalMap[gadget::KEY_V] = &mKeyPress_KEY_V;
    mKeyReleaseSignalMap[gadget::KEY_V] = &mKeyRelease_KEY_V;

    mKeyPressSignalMap[gadget::KEY_W] = &mKeyPress_KEY_W;
    mKeyReleaseSignalMap[gadget::KEY_W] = &mKeyRelease_KEY_W;

    mKeyPressSignalMap[gadget::KEY_X] = &mKeyPress_KEY_X;
    mKeyReleaseSignalMap[gadget::KEY_X] = &mKeyRelease_KEY_X;

    mKeyPressSignalMap[gadget::KEY_Y] = &mKeyPress_KEY_Y;
    mKeyReleaseSignalMap[gadget::KEY_Y] = &mKeyRelease_KEY_Y;

    mKeyPressSignalMap[gadget::KEY_Z] = &mKeyPress_KEY_Z;
    mKeyReleaseSignalMap[gadget::KEY_Z] = &mKeyRelease_KEY_Z;

    mKeyPressSignalMap[gadget::KEY_ESC] = &mKeyPress_KEY_ESC;
    mKeyReleaseSignalMap[gadget::KEY_ESC] = &mKeyRelease_KEY_ESC;

    mKeyPressSignalMap[gadget::KEY_TAB] = &mKeyPress_KEY_TAB;
    mKeyReleaseSignalMap[gadget::KEY_TAB] = &mKeyRelease_KEY_TAB;

    mKeyPressSignalMap[gadget::KEY_BACKTAB] = &mKeyPress_KEY_BACKTAB;
    mKeyReleaseSignalMap[gadget::KEY_BACKTAB] = &mKeyRelease_KEY_BACKTAB;

    mKeyPressSignalMap[gadget::KEY_BACKSPACE] = &mKeyPress_KEY_BACKSPACE;
    mKeyReleaseSignalMap[gadget::KEY_BACKSPACE] = &mKeyRelease_KEY_BACKSPACE;

    mKeyPressSignalMap[gadget::KEY_RETURN] = &mKeyPress_KEY_RETURN;
    mKeyReleaseSignalMap[gadget::KEY_RETURN] = &mKeyRelease_KEY_RETURN;

    mKeyPressSignalMap[gadget::KEY_ENTER] = &mKeyPress_KEY_ENTER;
    mKeyReleaseSignalMap[gadget::KEY_ENTER] = &mKeyRelease_KEY_ENTER;

    mKeyPressSignalMap[gadget::KEY_INSERT] = &mKeyPress_KEY_INSERT;
    mKeyReleaseSignalMap[gadget::KEY_INSERT] = &mKeyRelease_KEY_INSERT;

    mKeyPressSignalMap[gadget::KEY_DELETE] = &mKeyPress_KEY_DELETE;
    mKeyReleaseSignalMap[gadget::KEY_DELETE] = &mKeyRelease_KEY_DELETE;

    mKeyPressSignalMap[gadget::KEY_PAUSE] = &mKeyPress_KEY_PAUSE;
    mKeyReleaseSignalMap[gadget::KEY_PAUSE] = &mKeyRelease_KEY_PAUSE;

    mKeyPressSignalMap[gadget::KEY_PRINT] = &mKeyPress_KEY_PRINT;
    mKeyReleaseSignalMap[gadget::KEY_PRINT] = &mKeyRelease_KEY_PRINT;

    mKeyPressSignalMap[gadget::KEY_SYSREQ] = &mKeyPress_KEY_SYSREQ;
    mKeyReleaseSignalMap[gadget::KEY_SYSREQ] = &mKeyRelease_KEY_SYSREQ;

    mKeyPressSignalMap[gadget::KEY_HOME] = &mKeyPress_KEY_HOME;
    mKeyReleaseSignalMap[gadget::KEY_HOME] = &mKeyRelease_KEY_HOME;

    mKeyPressSignalMap[gadget::KEY_END] = &mKeyPress_KEY_END;
    mKeyReleaseSignalMap[gadget::KEY_END] = &mKeyRelease_KEY_END;

    mKeyPressSignalMap[gadget::KEY_PRIOR] = &mKeyPress_KEY_PRIOR;
    mKeyReleaseSignalMap[gadget::KEY_PRIOR] = &mKeyRelease_KEY_PRIOR;

    mKeyPressSignalMap[gadget::KEY_NEXT] = &mKeyPress_KEY_NEXT;
    mKeyReleaseSignalMap[gadget::KEY_NEXT] = &mKeyRelease_KEY_NEXT;

    mKeyPressSignalMap[gadget::KEY_CAPS_LOCK] = &mKeyPress_KEY_CAPS_LOCK;
    mKeyReleaseSignalMap[gadget::KEY_CAPS_LOCK] = &mKeyRelease_KEY_CAPS_LOCK;

    mKeyPressSignalMap[gadget::KEY_NUM_LOCK] = &mKeyPress_KEY_NUM_LOCK;
    mKeyReleaseSignalMap[gadget::KEY_NUM_LOCK] = &mKeyRelease_KEY_NUM_LOCK;

    mKeyPressSignalMap[gadget::KEY_SCROLL_LOCK] = &mKeyPress_KEY_SCROLL_LOCK;
    mKeyReleaseSignalMap[gadget::KEY_SCROLL_LOCK] = &mKeyRelease_KEY_SCROLL_LOCK;

    mKeyPressSignalMap[gadget::KEY_F1] = &mKeyPress_KEY_F1;
    mKeyReleaseSignalMap[gadget::KEY_F1] = &mKeyRelease_KEY_F1;

    mKeyPressSignalMap[gadget::KEY_F2] = &mKeyPress_KEY_F2;
    mKeyReleaseSignalMap[gadget::KEY_F2] = &mKeyRelease_KEY_F2;

    mKeyPressSignalMap[gadget::KEY_F3] = &mKeyPress_KEY_F3;
    mKeyReleaseSignalMap[gadget::KEY_F3] = &mKeyRelease_KEY_F3;

    mKeyPressSignalMap[gadget::KEY_F4] = &mKeyPress_KEY_F4;
    mKeyReleaseSignalMap[gadget::KEY_F4] = &mKeyRelease_KEY_F4;

    mKeyPressSignalMap[gadget::KEY_F5] = &mKeyPress_KEY_F5;
    mKeyReleaseSignalMap[gadget::KEY_F5] = &mKeyRelease_KEY_F5;

    mKeyPressSignalMap[gadget::KEY_F6] = &mKeyPress_KEY_F6;
    mKeyReleaseSignalMap[gadget::KEY_F6] = &mKeyRelease_KEY_F6;

    mKeyPressSignalMap[gadget::KEY_F7] = &mKeyPress_KEY_F7;
    mKeyReleaseSignalMap[gadget::KEY_F7] = &mKeyRelease_KEY_F7;

    mKeyPressSignalMap[gadget::KEY_F8] = &mKeyPress_KEY_F8;
    mKeyReleaseSignalMap[gadget::KEY_F8] = &mKeyRelease_KEY_F8;

    mKeyPressSignalMap[gadget::KEY_F9] = &mKeyPress_KEY_F9;
    mKeyReleaseSignalMap[gadget::KEY_F9] = &mKeyRelease_KEY_F9;

    mKeyPressSignalMap[gadget::KEY_F10] = &mKeyPress_KEY_F10;
    mKeyReleaseSignalMap[gadget::KEY_F10] = &mKeyRelease_KEY_F10;

    mKeyPressSignalMap[gadget::KEY_F11] = &mKeyPress_KEY_F11;
    mKeyReleaseSignalMap[gadget::KEY_F11] = &mKeyRelease_KEY_F11;

    mKeyPressSignalMap[gadget::KEY_F12] = &mKeyPress_KEY_F12;
    mKeyReleaseSignalMap[gadget::KEY_F12] = &mKeyRelease_KEY_F12;

    mKeyPressSignalMap[gadget::KEY_F13] = &mKeyPress_KEY_F13;
    mKeyReleaseSignalMap[gadget::KEY_F13] = &mKeyRelease_KEY_F13;

    mKeyPressSignalMap[gadget::KEY_F14] = &mKeyPress_KEY_F14;
    mKeyReleaseSignalMap[gadget::KEY_F14] = &mKeyRelease_KEY_F14;

    mKeyPressSignalMap[gadget::KEY_F15] = &mKeyPress_KEY_F15;
    mKeyReleaseSignalMap[gadget::KEY_F15] = &mKeyRelease_KEY_F15;

    mKeyPressSignalMap[gadget::KEY_F16] = &mKeyPress_KEY_F16;
    mKeyReleaseSignalMap[gadget::KEY_F16] = &mKeyRelease_KEY_F16;

    mKeyPressSignalMap[gadget::KEY_F17] = &mKeyPress_KEY_F17;
    mKeyReleaseSignalMap[gadget::KEY_F17] = &mKeyRelease_KEY_F17;

    mKeyPressSignalMap[gadget::KEY_F18] = &mKeyPress_KEY_F18;
    mKeyReleaseSignalMap[gadget::KEY_F18] = &mKeyRelease_KEY_F18;

    mKeyPressSignalMap[gadget::KEY_F19] = &mKeyPress_KEY_F19;
    mKeyReleaseSignalMap[gadget::KEY_F19] = &mKeyRelease_KEY_F19;

    mKeyPressSignalMap[gadget::KEY_F20] = &mKeyPress_KEY_F20;
    mKeyReleaseSignalMap[gadget::KEY_F20] = &mKeyRelease_KEY_F20;

    mKeyPressSignalMap[gadget::KEY_F21] = &mKeyPress_KEY_F21;
    mKeyReleaseSignalMap[gadget::KEY_F21] = &mKeyRelease_KEY_F21;

    mKeyPressSignalMap[gadget::KEY_F22] = &mKeyPress_KEY_F22;
    mKeyReleaseSignalMap[gadget::KEY_F22] = &mKeyRelease_KEY_F22;

    mKeyPressSignalMap[gadget::KEY_F23] = &mKeyPress_KEY_F23;
    mKeyReleaseSignalMap[gadget::KEY_F23] = &mKeyRelease_KEY_F23;

    mKeyPressSignalMap[gadget::KEY_F24] = &mKeyPress_KEY_F24;
    mKeyReleaseSignalMap[gadget::KEY_F24] = &mKeyRelease_KEY_F24;

    mKeyPressSignalMap[gadget::KEY_F25] = &mKeyPress_KEY_F25;
    mKeyReleaseSignalMap[gadget::KEY_F25] = &mKeyRelease_KEY_F25;

    mKeyPressSignalMap[gadget::KEY_F26] = &mKeyPress_KEY_F26;
    mKeyReleaseSignalMap[gadget::KEY_F26] = &mKeyRelease_KEY_F26;

    mKeyPressSignalMap[gadget::KEY_F27] = &mKeyPress_KEY_F27;
    mKeyReleaseSignalMap[gadget::KEY_F27] = &mKeyRelease_KEY_F27;

    mKeyPressSignalMap[gadget::KEY_F28] = &mKeyPress_KEY_F28;
    mKeyReleaseSignalMap[gadget::KEY_F28] = &mKeyRelease_KEY_F28;

    mKeyPressSignalMap[gadget::KEY_F29] = &mKeyPress_KEY_F29;
    mKeyReleaseSignalMap[gadget::KEY_F29] = &mKeyRelease_KEY_F29;

    mKeyPressSignalMap[gadget::KEY_F30] = &mKeyPress_KEY_F30;
    mKeyReleaseSignalMap[gadget::KEY_F30] = &mKeyRelease_KEY_F30;

    mKeyPressSignalMap[gadget::KEY_F31] = &mKeyPress_KEY_F31;
    mKeyReleaseSignalMap[gadget::KEY_F31] = &mKeyRelease_KEY_F31;

    mKeyPressSignalMap[gadget::KEY_F32] = &mKeyPress_KEY_F32;
    mKeyReleaseSignalMap[gadget::KEY_F32] = &mKeyRelease_KEY_F32;

    mKeyPressSignalMap[gadget::KEY_F33] = &mKeyPress_KEY_F33;
    mKeyReleaseSignalMap[gadget::KEY_F33] = &mKeyRelease_KEY_F33;

    mKeyPressSignalMap[gadget::KEY_F34] = &mKeyPress_KEY_F34;
    mKeyReleaseSignalMap[gadget::KEY_F34] = &mKeyRelease_KEY_F34;

    mKeyPressSignalMap[gadget::KEY_F35] = &mKeyPress_KEY_F35;
    mKeyReleaseSignalMap[gadget::KEY_F35] = &mKeyRelease_KEY_F35;

    mKeyPressSignalMap[gadget::KEY_SUPER_L] = &mKeyPress_KEY_SUPER_L;
    mKeyReleaseSignalMap[gadget::KEY_SUPER_L] = &mKeyRelease_KEY_SUPER_L;

    mKeyPressSignalMap[gadget::KEY_SUPER_R] = &mKeyPress_KEY_SUPER_R;
    mKeyReleaseSignalMap[gadget::KEY_SUPER_R] = &mKeyRelease_KEY_SUPER_R;

    mKeyPressSignalMap[gadget::KEY_MENU] = &mKeyPress_KEY_MENU;
    mKeyReleaseSignalMap[gadget::KEY_MENU] = &mKeyRelease_KEY_MENU;

    mKeyPressSignalMap[gadget::KEY_HYPER_L] = &mKeyPress_KEY_HYPER_L;
    mKeyReleaseSignalMap[gadget::KEY_HYPER_L] = &mKeyRelease_KEY_HYPER_L;

    mKeyPressSignalMap[gadget::KEY_HYPER_R] = &mKeyPress_KEY_HYPER_R;
    mKeyReleaseSignalMap[gadget::KEY_HYPER_R] = &mKeyRelease_KEY_HYPER_R;

    mKeyPressSignalMap[gadget::KEY_HELP] = &mKeyPress_KEY_HELP;
    mKeyReleaseSignalMap[gadget::KEY_HELP] = &mKeyRelease_KEY_HELP;

    mKeyPressSignalMap[gadget::KEY_SPACE] = &mKeyPress_KEY_SPACE;
    mKeyReleaseSignalMap[gadget::KEY_SPACE] = &mKeyRelease_KEY_SPACE;

    mKeyPressSignalMap[gadget::KEY_ANY] = &mKeyPress_KEY_ANY;
    mKeyReleaseSignalMap[gadget::KEY_ANY] = &mKeyRelease_KEY_ANY;

    mKeyPressSignalMap[gadget::KEY_EXCLAM] = &mKeyPress_KEY_EXCLAM;
    mKeyReleaseSignalMap[gadget::KEY_EXCLAM] = &mKeyRelease_KEY_EXCLAM;

    mKeyPressSignalMap[gadget::KEY_QUOTE_DBL] = &mKeyPress_KEY_QUOTE_DBL;
    mKeyReleaseSignalMap[gadget::KEY_QUOTE_DBL] = &mKeyRelease_KEY_QUOTE_DBL;

    mKeyPressSignalMap[gadget::KEY_NUMBER_SIGN] = &mKeyPress_KEY_NUMBER_SIGN;
    mKeyReleaseSignalMap[gadget::KEY_NUMBER_SIGN] = &mKeyRelease_KEY_NUMBER_SIGN;

    mKeyPressSignalMap[gadget::KEY_DOLLAR] = &mKeyPress_KEY_DOLLAR;
    mKeyReleaseSignalMap[gadget::KEY_DOLLAR] = &mKeyRelease_KEY_DOLLAR;

    mKeyPressSignalMap[gadget::KEY_PERCENT] = &mKeyPress_KEY_PERCENT;
    mKeyReleaseSignalMap[gadget::KEY_PERCENT] = &mKeyRelease_KEY_PERCENT;

    mKeyPressSignalMap[gadget::KEY_AMPERSAND] = &mKeyPress_KEY_AMPERSAND;
    mKeyReleaseSignalMap[gadget::KEY_AMPERSAND] = &mKeyRelease_KEY_AMPERSAND;

    mKeyPressSignalMap[gadget::KEY_APOSTROPHE] = &mKeyPress_KEY_APOSTROPHE;
    mKeyReleaseSignalMap[gadget::KEY_APOSTROPHE] = &mKeyRelease_KEY_APOSTROPHE;

    mKeyPressSignalMap[gadget::KEY_PAREN_LEFT] = &mKeyPress_KEY_PAREN_LEFT;
    mKeyReleaseSignalMap[gadget::KEY_PAREN_LEFT] = &mKeyRelease_KEY_PAREN_LEFT;

    mKeyPressSignalMap[gadget::KEY_PAREN_RIGHT] = &mKeyPress_KEY_PAREN_RIGHT;
    mKeyReleaseSignalMap[gadget::KEY_PAREN_RIGHT] = &mKeyRelease_KEY_PAREN_RIGHT;

    mKeyPressSignalMap[gadget::KEY_ASTERISK] = &mKeyPress_KEY_ASTERISK;
    mKeyReleaseSignalMap[gadget::KEY_ASTERISK] = &mKeyRelease_KEY_ASTERISK;

    mKeyPressSignalMap[gadget::KEY_PLUS] = &mKeyPress_KEY_PLUS;
    mKeyReleaseSignalMap[gadget::KEY_PLUS] = &mKeyRelease_KEY_PLUS;

    mKeyPressSignalMap[gadget::KEY_COMMA] = &mKeyPress_KEY_COMMA;
    mKeyReleaseSignalMap[gadget::KEY_COMMA] = &mKeyRelease_KEY_COMMA;

    mKeyPressSignalMap[gadget::KEY_MINUS] = &mKeyPress_KEY_MINUS;
    mKeyReleaseSignalMap[gadget::KEY_MINUS] = &mKeyRelease_KEY_MINUS;

    mKeyPressSignalMap[gadget::KEY_PERIOD] = &mKeyPress_KEY_PERIOD;
    mKeyReleaseSignalMap[gadget::KEY_PERIOD] = &mKeyRelease_KEY_PERIOD;

    mKeyPressSignalMap[gadget::KEY_SLASH] = &mKeyPress_KEY_SLASH;
    mKeyReleaseSignalMap[gadget::KEY_SLASH] = &mKeyRelease_KEY_SLASH;

    mKeyPressSignalMap[gadget::KEY_COLON] = &mKeyPress_KEY_COLON;
    mKeyReleaseSignalMap[gadget::KEY_COLON] = &mKeyRelease_KEY_COLON;

    mKeyPressSignalMap[gadget::KEY_SEMICOLON] = &mKeyPress_KEY_SEMICOLON;
    mKeyReleaseSignalMap[gadget::KEY_SEMICOLON] = &mKeyRelease_KEY_SEMICOLON;

    mKeyPressSignalMap[gadget::KEY_LESS] = &mKeyPress_KEY_LESS;
    mKeyReleaseSignalMap[gadget::KEY_LESS] = &mKeyRelease_KEY_LESS;

    mKeyPressSignalMap[gadget::KEY_EQUAL] = &mKeyPress_KEY_EQUAL;
    mKeyReleaseSignalMap[gadget::KEY_EQUAL] = &mKeyRelease_KEY_EQUAL;

    mKeyPressSignalMap[gadget::KEY_GREATER] = &mKeyPress_KEY_GREATER;
    mKeyReleaseSignalMap[gadget::KEY_GREATER] = &mKeyRelease_KEY_GREATER;

    mKeyPressSignalMap[gadget::KEY_QUESTION] = &mKeyPress_KEY_QUESTION;
    mKeyReleaseSignalMap[gadget::KEY_QUESTION] = &mKeyRelease_KEY_QUESTION;

    mKeyPressSignalMap[gadget::KEY_AT] = &mKeyPress_KEY_AT;
    mKeyReleaseSignalMap[gadget::KEY_AT] = &mKeyRelease_KEY_AT;

    mKeyPressSignalMap[gadget::KEY_BRACKET_LEFT] = &mKeyPress_KEY_BRACKET_LEFT;
    mKeyReleaseSignalMap[gadget::KEY_BRACKET_LEFT] = &mKeyRelease_KEY_BRACKET_LEFT;

    mKeyPressSignalMap[gadget::KEY_BACKSLASH] = &mKeyPress_KEY_BACKSLASH;
    mKeyReleaseSignalMap[gadget::KEY_BACKSLASH] = &mKeyRelease_KEY_BACKSLASH;

    mKeyPressSignalMap[gadget::KEY_BRACKET_RIGHT] = &mKeyPress_KEY_BRACKET_RIGHT;
    mKeyReleaseSignalMap[gadget::KEY_BRACKET_RIGHT] = &mKeyRelease_KEY_BRACKET_RIGHT;

    mKeyPressSignalMap[gadget::KEY_ASCII_CIRCUM] = &mKeyPress_KEY_ASCII_CIRCUM;
    mKeyReleaseSignalMap[gadget::KEY_ASCII_CIRCUM] = &mKeyRelease_KEY_ASCII_CIRCUM;

    mKeyPressSignalMap[gadget::KEY_UNDERSCORE] = &mKeyPress_KEY_UNDERSCORE;
    mKeyReleaseSignalMap[gadget::KEY_UNDERSCORE] = &mKeyRelease_KEY_UNDERSCORE;

    mKeyPressSignalMap[gadget::KEY_QUOTE_LEFT] = &mKeyPress_KEY_QUOTE_LEFT;
    mKeyReleaseSignalMap[gadget::KEY_QUOTE_LEFT] = &mKeyRelease_KEY_QUOTE_LEFT;

    mKeyPressSignalMap[gadget::KEY_BRACE_LEFT] = &mKeyPress_KEY_BRACE_LEFT;
    mKeyReleaseSignalMap[gadget::KEY_BRACE_LEFT] = &mKeyRelease_KEY_BRACE_LEFT;

    mKeyPressSignalMap[gadget::KEY_BAR] = &mKeyPress_KEY_BAR;
    mKeyReleaseSignalMap[gadget::KEY_BAR] = &mKeyRelease_KEY_BAR;

    mKeyPressSignalMap[gadget::KEY_BRACE_RIGHT] = &mKeyPress_KEY_BRACE_RIGHT;
    mKeyReleaseSignalMap[gadget::KEY_BRACE_RIGHT] = &mKeyRelease_KEY_BRACE_RIGHT;

    mKeyPressSignalMap[gadget::KEY_ASCII_TILDE] = &mKeyPress_KEY_ASCII_TILDE;
    mKeyReleaseSignalMap[gadget::KEY_ASCII_TILDE] = &mKeyRelease_KEY_ASCII_TILDE;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RegisterKeySignals()
{
    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_NONE ),
            "KeyboardMouse.KeyPress_KEY_NONE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_NONE ),
            "KeyboardMouse.KeyRelease_KEY_NONE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_UP ),
            "KeyboardMouse.KeyPress_KEY_UP", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_UP ),
            "KeyboardMouse.KeyRelease_KEY_UP", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_DOWN ),
            "KeyboardMouse.KeyPress_KEY_DOWN", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_DOWN ),
            "KeyboardMouse.KeyRelease_KEY_DOWN", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_LEFT ),
            "KeyboardMouse.KeyPress_KEY_LEFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_LEFT ),
            "KeyboardMouse.KeyRelease_KEY_LEFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_RIGHT ),
            "KeyboardMouse.KeyPress_KEY_RIGHT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_RIGHT ),
            "KeyboardMouse.KeyRelease_KEY_RIGHT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SHIFT ),
            "KeyboardMouse.KeyPress_KEY_SHIFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SHIFT ),
            "KeyboardMouse.KeyRelease_KEY_SHIFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_CTRL ),
            "KeyboardMouse.KeyPress_KEY_CTRL", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_CTRL ),
            "KeyboardMouse.KeyRelease_KEY_CTRL", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ALT ),
            "KeyboardMouse.KeyPress_KEY_ALT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ALT ),
            "KeyboardMouse.KeyRelease_KEY_ALT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_COMMAND ),
            "KeyboardMouse.KeyPress_KEY_COMMAND", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_COMMAND ),
            "KeyboardMouse.KeyRelease_KEY_COMMAND", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_1 ),
            "KeyboardMouse.KeyPress_KEY_1", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_1 ),
            "KeyboardMouse.KeyRelease_KEY_1", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_2 ),
            "KeyboardMouse.KeyPress_KEY_2", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_2 ),
            "KeyboardMouse.KeyRelease_KEY_2", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_3 ),
            "KeyboardMouse.KeyPress_KEY_3", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_3 ),
            "KeyboardMouse.KeyRelease_KEY_3", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_4 ),
            "KeyboardMouse.KeyPress_KEY_4", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_4 ),
            "KeyboardMouse.KeyRelease_KEY_4", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_5 ),
            "KeyboardMouse.KeyPress_KEY_5", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_5 ),
            "KeyboardMouse.KeyRelease_KEY_5", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_6 ),
            "KeyboardMouse.KeyPress_KEY_6", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_6 ),
            "KeyboardMouse.KeyRelease_KEY_6", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_7 ),
            "KeyboardMouse.KeyPress_KEY_7", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_7 ),
            "KeyboardMouse.KeyRelease_KEY_7", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_8 ),
            "KeyboardMouse.KeyPress_KEY_8", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_8 ),
            "KeyboardMouse.KeyRelease_KEY_8", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_9 ),
            "KeyboardMouse.KeyPress_KEY_9", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_9 ),
            "KeyboardMouse.KeyRelease_KEY_9", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_0 ),
            "KeyboardMouse.KeyPress_KEY_0", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_0 ),
            "KeyboardMouse.KeyRelease_KEY_0", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_A ),
            "KeyboardMouse.KeyPress_KEY_A", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_A ),
            "KeyboardMouse.KeyRelease_KEY_A", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_B ),
            "KeyboardMouse.KeyPress_KEY_B", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_B ),
            "KeyboardMouse.KeyRelease_KEY_B", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_C ),
            "KeyboardMouse.KeyPress_KEY_C", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_C ),
            "KeyboardMouse.KeyRelease_KEY_C", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_D ),
            "KeyboardMouse.KeyPress_KEY_D", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_D ),
            "KeyboardMouse.KeyRelease_KEY_D", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_E ),
            "KeyboardMouse.KeyPress_KEY_E", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_E ),
            "KeyboardMouse.KeyRelease_KEY_E", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F ),
            "KeyboardMouse.KeyPress_KEY_F", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F ),
            "KeyboardMouse.KeyRelease_KEY_F", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_G ),
            "KeyboardMouse.KeyPress_KEY_G", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_G ),
            "KeyboardMouse.KeyRelease_KEY_G", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_H ),
            "KeyboardMouse.KeyPress_KEY_H", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_H ),
            "KeyboardMouse.KeyRelease_KEY_H", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_I ),
            "KeyboardMouse.KeyPress_KEY_I", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_I ),
            "KeyboardMouse.KeyRelease_KEY_I", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_J ),
            "KeyboardMouse.KeyPress_KEY_J", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_J ),
            "KeyboardMouse.KeyRelease_KEY_J", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_K ),
            "KeyboardMouse.KeyPress_KEY_K", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_K ),
            "KeyboardMouse.KeyRelease_KEY_K", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_L ),
            "KeyboardMouse.KeyPress_KEY_L", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_L ),
            "KeyboardMouse.KeyRelease_KEY_L", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_M ),
            "KeyboardMouse.KeyPress_KEY_M", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_M ),
            "KeyboardMouse.KeyRelease_KEY_M", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_N ),
            "KeyboardMouse.KeyPress_KEY_N", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_N ),
            "KeyboardMouse.KeyRelease_KEY_N", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_O ),
            "KeyboardMouse.KeyPress_KEY_O", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_O ),
            "KeyboardMouse.KeyRelease_KEY_O", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_P ),
            "KeyboardMouse.KeyPress_KEY_P", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_P ),
            "KeyboardMouse.KeyRelease_KEY_P", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_Q ),
            "KeyboardMouse.KeyPress_KEY_Q", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_Q ),
            "KeyboardMouse.KeyRelease_KEY_Q", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_R ),
            "KeyboardMouse.KeyPress_KEY_R", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_R ),
            "KeyboardMouse.KeyRelease_KEY_R", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_S ),
            "KeyboardMouse.KeyPress_KEY_S", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_S ),
            "KeyboardMouse.KeyRelease_KEY_S", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_T ),
            "KeyboardMouse.KeyPress_KEY_T", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_T ),
            "KeyboardMouse.KeyRelease_KEY_T", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_U ),
            "KeyboardMouse.KeyPress_KEY_U", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_U ),
            "KeyboardMouse.KeyRelease_KEY_U", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_V ),
            "KeyboardMouse.KeyPress_KEY_V", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_V ),
            "KeyboardMouse.KeyRelease_KEY_V", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_W ),
            "KeyboardMouse.KeyPress_KEY_W", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_W ),
            "KeyboardMouse.KeyRelease_KEY_W", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_X ),
            "KeyboardMouse.KeyPress_KEY_X", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_X ),
            "KeyboardMouse.KeyRelease_KEY_X", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_Y ),
            "KeyboardMouse.KeyPress_KEY_Y", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_Y ),
            "KeyboardMouse.KeyRelease_KEY_Y", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_Z ),
            "KeyboardMouse.KeyPress_KEY_Z", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_Z ),
            "KeyboardMouse.KeyRelease_KEY_Z", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ESC ),
            "KeyboardMouse.KeyPress_KEY_ESC", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ESC ),
            "KeyboardMouse.KeyRelease_KEY_ESC", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_TAB ),
            "KeyboardMouse.KeyPress_KEY_TAB", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_TAB ),
            "KeyboardMouse.KeyRelease_KEY_TAB", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BACKTAB ),
            "KeyboardMouse.KeyPress_KEY_BACKTAB", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BACKTAB ),
            "KeyboardMouse.KeyRelease_KEY_BACKTAB", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BACKSPACE ),
            "KeyboardMouse.KeyPress_KEY_BACKSPACE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BACKSPACE ),
            "KeyboardMouse.KeyRelease_KEY_BACKSPACE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_RETURN ),
            "KeyboardMouse.KeyPress_KEY_RETURN", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_RETURN ),
            "KeyboardMouse.KeyRelease_KEY_RETURN", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ENTER ),
            "KeyboardMouse.KeyPress_KEY_ENTER", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ENTER ),
            "KeyboardMouse.KeyRelease_KEY_ENTER", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_INSERT ),
            "KeyboardMouse.KeyPress_KEY_INSERT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_INSERT ),
            "KeyboardMouse.KeyRelease_KEY_INSERT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_DELETE ),
            "KeyboardMouse.KeyPress_KEY_DELETE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_DELETE ),
            "KeyboardMouse.KeyRelease_KEY_DELETE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PAUSE ),
            "KeyboardMouse.KeyPress_KEY_PAUSE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PAUSE ),
            "KeyboardMouse.KeyRelease_KEY_PAUSE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PRINT ),
            "KeyboardMouse.KeyPress_KEY_PRINT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PRINT ),
            "KeyboardMouse.KeyRelease_KEY_PRINT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SYSREQ ),
            "KeyboardMouse.KeyPress_KEY_SYSREQ", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SYSREQ ),
            "KeyboardMouse.KeyRelease_KEY_SYSREQ", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_HOME ),
            "KeyboardMouse.KeyPress_KEY_HOME", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_HOME ),
            "KeyboardMouse.KeyRelease_KEY_HOME", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_END ),
            "KeyboardMouse.KeyPress_KEY_END", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_END ),
            "KeyboardMouse.KeyRelease_KEY_END", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PRIOR ),
            "KeyboardMouse.KeyPress_KEY_PRIOR", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PRIOR ),
            "KeyboardMouse.KeyRelease_KEY_PRIOR", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_NEXT ),
            "KeyboardMouse.KeyPress_KEY_NEXT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_NEXT ),
            "KeyboardMouse.KeyRelease_KEY_NEXT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_CAPS_LOCK ),
            "KeyboardMouse.KeyPress_KEY_CAPS_LOCK", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_CAPS_LOCK ),
            "KeyboardMouse.KeyRelease_KEY_CAPS_LOCK", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_NUM_LOCK ),
            "KeyboardMouse.KeyPress_KEY_NUM_LOCK", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_NUM_LOCK ),
            "KeyboardMouse.KeyRelease_KEY_NUM_LOCK", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SCROLL_LOCK ),
            "KeyboardMouse.KeyPress_KEY_SCROLL_LOCK", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SCROLL_LOCK ),
            "KeyboardMouse.KeyRelease_KEY_SCROLL_LOCK", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F1 ),
            "KeyboardMouse.KeyPress_KEY_F1", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F1 ),
            "KeyboardMouse.KeyRelease_KEY_F1", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F2 ),
            "KeyboardMouse.KeyPress_KEY_F2", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F2 ),
            "KeyboardMouse.KeyRelease_KEY_F2", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F3 ),
            "KeyboardMouse.KeyPress_KEY_F3", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F3 ),
            "KeyboardMouse.KeyRelease_KEY_F3", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F4 ),
            "KeyboardMouse.KeyPress_KEY_F4", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F4 ),
            "KeyboardMouse.KeyRelease_KEY_F4", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F5 ),
            "KeyboardMouse.KeyPress_KEY_F5", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F5 ),
            "KeyboardMouse.KeyRelease_KEY_F5", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F6 ),
            "KeyboardMouse.KeyPress_KEY_F6", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F6 ),
            "KeyboardMouse.KeyRelease_KEY_F6", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F7 ),
            "KeyboardMouse.KeyPress_KEY_F7", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F7 ),
            "KeyboardMouse.KeyRelease_KEY_F7", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F8 ),
            "KeyboardMouse.KeyPress_KEY_F8", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F8 ),
            "KeyboardMouse.KeyRelease_KEY_F8", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F9 ),
            "KeyboardMouse.KeyPress_KEY_F9", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F9 ),
            "KeyboardMouse.KeyRelease_KEY_F9", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F10 ),
            "KeyboardMouse.KeyPress_KEY_F10", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F10 ),
            "KeyboardMouse.KeyRelease_KEY_F10", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F11 ),
            "KeyboardMouse.KeyPress_KEY_F11", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F11 ),
            "KeyboardMouse.KeyRelease_KEY_F11", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F12 ),
            "KeyboardMouse.KeyPress_KEY_F12", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F12 ),
            "KeyboardMouse.KeyRelease_KEY_F12", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F13 ),
            "KeyboardMouse.KeyPress_KEY_F13", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F13 ),
            "KeyboardMouse.KeyRelease_KEY_F13", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F14 ),
            "KeyboardMouse.KeyPress_KEY_F14", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F14 ),
            "KeyboardMouse.KeyRelease_KEY_F14", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F15 ),
            "KeyboardMouse.KeyPress_KEY_F15", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F15 ),
            "KeyboardMouse.KeyRelease_KEY_F15", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F16 ),
            "KeyboardMouse.KeyPress_KEY_F16", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F16 ),
            "KeyboardMouse.KeyRelease_KEY_F16", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F17 ),
            "KeyboardMouse.KeyPress_KEY_F17", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F17 ),
            "KeyboardMouse.KeyRelease_KEY_F17", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F18 ),
            "KeyboardMouse.KeyPress_KEY_F18", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F18 ),
            "KeyboardMouse.KeyRelease_KEY_F18", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F19 ),
            "KeyboardMouse.KeyPress_KEY_F19", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F19 ),
            "KeyboardMouse.KeyRelease_KEY_F19", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F20 ),
            "KeyboardMouse.KeyPress_KEY_F20", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F20 ),
            "KeyboardMouse.KeyRelease_KEY_F20", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F21 ),
            "KeyboardMouse.KeyPress_KEY_F21", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F21 ),
            "KeyboardMouse.KeyRelease_KEY_F21", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F22 ),
            "KeyboardMouse.KeyPress_KEY_F22", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F22 ),
            "KeyboardMouse.KeyRelease_KEY_F22", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F23 ),
            "KeyboardMouse.KeyPress_KEY_F23", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F23 ),
            "KeyboardMouse.KeyRelease_KEY_F23", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F24 ),
            "KeyboardMouse.KeyPress_KEY_F24", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F24 ),
            "KeyboardMouse.KeyRelease_KEY_F24", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F25 ),
            "KeyboardMouse.KeyPress_KEY_F25", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F25 ),
            "KeyboardMouse.KeyRelease_KEY_F25", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F26 ),
            "KeyboardMouse.KeyPress_KEY_F26", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F26 ),
            "KeyboardMouse.KeyRelease_KEY_F26", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F27 ),
            "KeyboardMouse.KeyPress_KEY_F27", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F27 ),
            "KeyboardMouse.KeyRelease_KEY_F27", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F28 ),
            "KeyboardMouse.KeyPress_KEY_F28", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F28 ),
            "KeyboardMouse.KeyRelease_KEY_F28", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F29 ),
            "KeyboardMouse.KeyPress_KEY_F29", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F29 ),
            "KeyboardMouse.KeyRelease_KEY_F29", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F30 ),
            "KeyboardMouse.KeyPress_KEY_F30", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F30 ),
            "KeyboardMouse.KeyRelease_KEY_F30", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F31 ),
            "KeyboardMouse.KeyPress_KEY_F31", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F31 ),
            "KeyboardMouse.KeyRelease_KEY_F31", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F32 ),
            "KeyboardMouse.KeyPress_KEY_F32", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F32 ),
            "KeyboardMouse.KeyRelease_KEY_F32", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F33 ),
            "KeyboardMouse.KeyPress_KEY_F33", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F33 ),
            "KeyboardMouse.KeyRelease_KEY_F33", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F34 ),
            "KeyboardMouse.KeyPress_KEY_F34", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F34 ),
            "KeyboardMouse.KeyRelease_KEY_F34", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_F35 ),
            "KeyboardMouse.KeyPress_KEY_F35", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_F35 ),
            "KeyboardMouse.KeyRelease_KEY_F35", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SUPER_L ),
            "KeyboardMouse.KeyPress_KEY_SUPER_L", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SUPER_L ),
            "KeyboardMouse.KeyRelease_KEY_SUPER_L", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SUPER_R ),
            "KeyboardMouse.KeyPress_KEY_SUPER_R", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SUPER_R ),
            "KeyboardMouse.KeyRelease_KEY_SUPER_R", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_MENU ),
            "KeyboardMouse.KeyPress_KEY_MENU", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_MENU ),
            "KeyboardMouse.KeyRelease_KEY_MENU", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_HYPER_L ),
            "KeyboardMouse.KeyPress_KEY_HYPER_L", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_HYPER_L ),
            "KeyboardMouse.KeyRelease_KEY_HYPER_L", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_HYPER_R ),
            "KeyboardMouse.KeyPress_KEY_HYPER_R", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_HYPER_R ),
            "KeyboardMouse.KeyRelease_KEY_HYPER_R", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_HELP ),
            "KeyboardMouse.KeyPress_KEY_HELP", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_HELP ),
            "KeyboardMouse.KeyRelease_KEY_HELP", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SPACE ),
            "KeyboardMouse.KeyPress_KEY_SPACE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SPACE ),
            "KeyboardMouse.KeyRelease_KEY_SPACE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ANY ),
            "KeyboardMouse.KeyPress_KEY_ANY", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ANY ),
            "KeyboardMouse.KeyRelease_KEY_ANY", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_EXCLAM ),
            "KeyboardMouse.KeyPress_KEY_EXCLAM", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_EXCLAM ),
            "KeyboardMouse.KeyRelease_KEY_EXCLAM", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_QUOTE_DBL ),
            "KeyboardMouse.KeyPress_KEY_QUOTE_DBL", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_QUOTE_DBL ),
            "KeyboardMouse.KeyRelease_KEY_QUOTE_DBL", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_NUMBER_SIGN ),
            "KeyboardMouse.KeyPress_KEY_NUMBER_SIGN", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_NUMBER_SIGN ),
            "KeyboardMouse.KeyRelease_KEY_NUMBER_SIGN", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_DOLLAR ),
            "KeyboardMouse.KeyPress_KEY_DOLLAR", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_DOLLAR ),
            "KeyboardMouse.KeyRelease_KEY_DOLLAR", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PERCENT ),
            "KeyboardMouse.KeyPress_KEY_PERCENT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PERCENT ),
            "KeyboardMouse.KeyRelease_KEY_PERCENT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_AMPERSAND ),
            "KeyboardMouse.KeyPress_KEY_AMPERSAND", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_AMPERSAND ),
            "KeyboardMouse.KeyRelease_KEY_AMPERSAND", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_APOSTROPHE ),
            "KeyboardMouse.KeyPress_KEY_APOSTROPHE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_APOSTROPHE ),
            "KeyboardMouse.KeyRelease_KEY_APOSTROPHE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PAREN_LEFT ),
            "KeyboardMouse.KeyPress_KEY_PAREN_LEFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PAREN_LEFT ),
            "KeyboardMouse.KeyRelease_KEY_PAREN_LEFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PAREN_RIGHT ),
            "KeyboardMouse.KeyPress_KEY_PAREN_RIGHT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PAREN_RIGHT ),
            "KeyboardMouse.KeyRelease_KEY_PAREN_RIGHT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ASTERISK ),
            "KeyboardMouse.KeyPress_KEY_ASTERISK", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ASTERISK ),
            "KeyboardMouse.KeyRelease_KEY_ASTERISK", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PLUS ),
            "KeyboardMouse.KeyPress_KEY_PLUS", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PLUS ),
            "KeyboardMouse.KeyRelease_KEY_PLUS", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_COMMA ),
            "KeyboardMouse.KeyPress_KEY_COMMA", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_COMMA ),
            "KeyboardMouse.KeyRelease_KEY_COMMA", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_MINUS ),
            "KeyboardMouse.KeyPress_KEY_MINUS", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_MINUS ),
            "KeyboardMouse.KeyRelease_KEY_MINUS", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_PERIOD ),
            "KeyboardMouse.KeyPress_KEY_PERIOD", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_PERIOD ),
            "KeyboardMouse.KeyRelease_KEY_PERIOD", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SLASH ),
            "KeyboardMouse.KeyPress_KEY_SLASH", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SLASH ),
            "KeyboardMouse.KeyRelease_KEY_SLASH", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_COLON ),
            "KeyboardMouse.KeyPress_KEY_COLON", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_COLON ),
            "KeyboardMouse.KeyRelease_KEY_COLON", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_SEMICOLON ),
            "KeyboardMouse.KeyPress_KEY_SEMICOLON", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_SEMICOLON ),
            "KeyboardMouse.KeyRelease_KEY_SEMICOLON", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_LESS ),
            "KeyboardMouse.KeyPress_KEY_LESS", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_LESS ),
            "KeyboardMouse.KeyRelease_KEY_LESS", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_EQUAL ),
            "KeyboardMouse.KeyPress_KEY_EQUAL", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_EQUAL ),
            "KeyboardMouse.KeyRelease_KEY_EQUAL", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_GREATER ),
            "KeyboardMouse.KeyPress_KEY_GREATER", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_GREATER ),
            "KeyboardMouse.KeyRelease_KEY_GREATER", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_QUESTION ),
            "KeyboardMouse.KeyPress_KEY_QUESTION", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_QUESTION ),
            "KeyboardMouse.KeyRelease_KEY_QUESTION", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_AT ),
            "KeyboardMouse.KeyPress_KEY_AT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_AT ),
            "KeyboardMouse.KeyRelease_KEY_AT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BRACKET_LEFT ),
            "KeyboardMouse.KeyPress_KEY_BRACKET_LEFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BRACKET_LEFT ),
            "KeyboardMouse.KeyRelease_KEY_BRACKET_LEFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BACKSLASH ),
            "KeyboardMouse.KeyPress_KEY_BACKSLASH", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BACKSLASH ),
            "KeyboardMouse.KeyRelease_KEY_BACKSLASH", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BRACKET_RIGHT ),
            "KeyboardMouse.KeyPress_KEY_BRACKET_RIGHT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BRACKET_RIGHT ),
            "KeyboardMouse.KeyRelease_KEY_BRACKET_RIGHT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ASCII_CIRCUM ),
            "KeyboardMouse.KeyPress_KEY_ASCII_CIRCUM", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ASCII_CIRCUM ),
            "KeyboardMouse.KeyRelease_KEY_ASCII_CIRCUM", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_UNDERSCORE ),
            "KeyboardMouse.KeyPress_KEY_UNDERSCORE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_UNDERSCORE ),
            "KeyboardMouse.KeyRelease_KEY_UNDERSCORE", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_QUOTE_LEFT ),
            "KeyboardMouse.KeyPress_KEY_QUOTE_LEFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_QUOTE_LEFT ),
            "KeyboardMouse.KeyRelease_KEY_QUOTE_LEFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BRACE_LEFT ),
            "KeyboardMouse.KeyPress_KEY_BRACE_LEFT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BRACE_LEFT ),
            "KeyboardMouse.KeyRelease_KEY_BRACE_LEFT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BAR ),
            "KeyboardMouse.KeyPress_KEY_BAR", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BAR ),
            "KeyboardMouse.KeyRelease_KEY_BAR", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_BRACE_RIGHT ),
            "KeyboardMouse.KeyPress_KEY_BRACE_RIGHT", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_BRACE_RIGHT ),
            "KeyboardMouse.KeyRelease_KEY_BRACE_RIGHT", eventmanager::EventManager::keyboard_SignalType );

    evm->RegisterSignal(
            new SignalWrapper< KeyPressSignal_type >( &mKeyPress_KEY_ASCII_TILDE ),
            "KeyboardMouse.KeyPress_KEY_ASCII_TILDE", eventmanager::EventManager::keyboard_SignalType );
    evm->RegisterSignal(
            new SignalWrapper< KeyReleaseSignal_type >( &mKeyRelease_KEY_ASCII_TILDE ),
            "KeyboardMouse.KeyRelease_KEY_ASCII_TILDE", eventmanager::EventManager::keyboard_SignalType );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetScreenCornerValues( std::map< std::string, double > values )
{

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SkyCam()
{

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelectionLine()
{

}
////////////////////////////////////////////////////////////////////////////////
gadget::KeyboardMousePtr KeyboardMouse::GetKeyboardMouseVRJDevice()
{
    return mKeyboardMousePtr;
}
////////////////////////////////////////////////////////////////////////////////
osgUtil::LineSegmentIntersector* KeyboardMouse::GetLineSegmentIntersector()
{
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetProcessSelection( bool processSelection )
{

}
////////////////////////////////////////////////////////////////////////////////
bool KeyboardMouse::GetMousePickEvent()
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
vrj::DisplayPtr const KeyboardMouse::GetCurrentDisplay(
    const gadget::InputArea* inputArea )
{
    const vrj::opengl::Window* window( NULL );
#if defined( VPR_OS_Darwin )
    const gadget::InputWindowCocoa* inputWindowCocoa =
    dynamic_cast< const gadget::InputWindowCocoa* >( inputArea );
    if( inputWindowCocoa )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowCocoa* windowCocoa =
    static_cast< const vrj::opengl::WindowCocoa* >( inputArea );
    //upcast
    window = static_cast< const vrj::opengl::Window* >( windowCocoa );
#elif defined( VPR_OS_Windows )
    const gadget::InputWindowWin32* inputWindowWin32 =
    dynamic_cast< const gadget::InputWindowWin32* >( inputArea );
    if( inputWindowWin32 )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowWin32* windowWin32 =
    static_cast< const vrj::opengl::WindowWin32* >( inputArea );
    //upcast
    window = dynamic_cast< const vrj::opengl::Window* >( windowWin32 );
#elif defined( VPR_OS_Linux )
    const gadget::InputWindowXWin* inputWindowXWin =
    dynamic_cast< const gadget::InputWindowXWin* >( inputArea );
    if( inputWindowXWin )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowXWin* windowXWin =
    static_cast< const vrj::opengl::WindowXWin* >( inputArea );
    //upcast
    window = dynamic_cast< const vrj::opengl::Window* >( windowXWin );
#endif
    
    if( window )
    {
        return window->getDisplay();
    }
    else
    {
        //Error output, this should never happen
        vprDEBUG( vesDBG, 2 ) 
        << "VPR OS is not defined properly in KeyboardMouse::GetCurrentDisplay." 
        << std::endl << vprDEBUG_FLUSH;
        return vrj::DisplayPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
bool KeyboardMouse::SetCurrentGLTransformInfo(
    const vrj::DisplayPtr display, bool isKeyEvent )
{
    //If current display is invalid, return
    if( display == vrj::DisplayPtr() )
    {
        m_sceneManager.SetCurrentGLTransformInfo( GLTransformInfoPtr() );
        return false;
    }
    
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        m_manipulatorManager.GetSceneManipulator();
    vrj::ViewportPtr viewport;
    //Iterate over the viewports
    for( unsigned int i = 0; i < display->getNumViewports(); ++i )
    {
        viewport = display->getViewport( i );
        m_currentGLTransformInfo = m_sceneManager.GetGLTransformInfo( viewport );
        if( m_currentGLTransformInfo == scenegraph::GLTransformInfoPtr() )
        {
            m_sceneManager.SetCurrentGLTransformInfo( GLTransformInfoPtr() );
            return false;
        }
        
        // ---------- This needs to be optimized ------------ //
        // --- Does not need to be set for every viewport --- //
        //const int& windowWidth = m_currentGLTransformInfo->GetWindowWidth();
        //const int& windowHeight = m_currentGLTransformInfo->GetWindowHeight();
        //SetWindowValues( windowWidth, windowHeight );
        // -------------------------------------------------- //
        
        if( isKeyEvent )
        {
            m_sceneManager.SetCurrentGLTransformInfo( m_currentGLTransformInfo );
            return true;
        }
        
        //Get dimensions of viewport in pixels
        const int& viewportOriginX = m_currentGLTransformInfo->GetViewportOriginX();
        const int& viewportOriginY = m_currentGLTransformInfo->GetViewportOriginY();
        const int& viewportWidth = m_currentGLTransformInfo->GetViewportWidth();
        const int& viewportHeight = m_currentGLTransformInfo->GetViewportHeight();
        
        //Check if mouse is inside viewport
        if( ( m_currX >= viewportOriginX ) &&
           ( m_currY >= viewportOriginY ) &&
           ( m_currX <= viewportOriginX + viewportWidth ) &&
           ( m_currY <= viewportOriginY + viewportHeight ) )
        {
            sceneManipulator->SetCurrentGLTransformInfo(
                                                        m_currentGLTransformInfo );
            m_sceneManager.SetCurrentGLTransformInfo( m_currentGLTransformInfo );            
            return true;
        }
    }
    
    m_sceneManager.SetCurrentGLTransformInfo( GLTransformInfoPtr() );
    return false;
}
////////////////////////////////////////////////////////////////////////////////
