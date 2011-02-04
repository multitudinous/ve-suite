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
    m_mouseInsideUI( true )
{
    //mHead.init( "VJHead" );

    // Connect to Juggler's new event handling interface
    m_mouseDoubleClickEventInterface.setClickTime(300);
    m_mouseDoubleClickEventInterface.init("VJKeyboard");
    m_mouseDoubleClickEventInterface.addCallback(boost::bind(&KeyboardMouse::onMouseDoubleClick, this, _1));

    m_keyboardMouseEventInterface.init("VJKeyboard");
    m_keyboardMouseEventInterface.addCallback(boost::bind(&KeyboardMouse::onKeyboardMouseEvent, this, _1));

    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    evm->RegisterSignal(
        new SignalWrapper< MouseMoveSignal_type >( &m_mouseMove ),
        "KeyboardMouse.MouseMove", eventmanager::EventManager::mouse_SignalType );

    evm->RegisterSignal(
        new SignalWrapper< MouseDoubleClickSignal_type >( &m_mouseDoubleClick ),
        "KeyboardMouse.DoubleClick", eventmanager::EventManager::button_SignalType );

    evm->RegisterSignal(
        new SignalWrapper< StartEndPointSignal_type >( &m_startEndPointSignal ),
        "KeyboardMouse.StartEndPoint", eventmanager::EventManager::unspecified_SignalType );
    
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
        
        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }
        
        //Send current Start and end points
        SetStartEndPoint( m_startPoint, m_endPoint );
        m_startEndPointSignal( m_startPoint, m_endPoint );

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

        const gadget::MouseEventPtr mouseEvt =
            boost::static_pointer_cast< gadget::MouseEvent >( event );

        m_currX = mouseEvt->getX();
        m_currY = mouseEvt->getY();

        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }        
        
        //Send current Start and end points
        SetStartEndPoint( m_startPoint, m_endPoint );
        m_startEndPointSignal( m_startPoint, m_endPoint );
        
        ButtonReleaseSignalMapType::iterator itr = mButtonReleaseSignalMap.find( mouseEvt->getButton() );
        if( itr != mButtonReleaseSignalMap.end() )
        {
            (*(itr->second))( mouseEvt->getButton(), mouseEvt->getX(), mouseEvt->getY(), mouseEvt->getState() );
        }

        break;
    }
    case gadget::MouseMoveEvent:
    {
        const gadget::MouseEventPtr mouseEvt =
            boost::static_pointer_cast< gadget::MouseEvent >( event );
        
        m_currX = mouseEvt->getX();
        m_currY = mouseEvt->getY();
        
        //Set the current GLTransfromInfo from the event
        if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
        {
            return;
        }        
        //std::cout<< "process move" << std::endl;

        //int buttonMask = mouseEvt->getState();
        //if( buttonMask&gadget::BUTTON1_MASK )
        {
            //Send current Start and end points - needed for constraint selection
            SetStartEndPoint( m_startPoint, m_endPoint );
            m_startEndPointSignal( m_startPoint, m_endPoint );
        }
        
        /*vprDEBUG( vesDBG, 2 )
            << "|\tKeyboardMouse::onKeyboardMouseEvent::MouseMoveEvent"
            << mouseEvt->getButton() << " " << mouseEvt->getX() << ", " << mouseEvt->getY()
            << ", 0, " << mouseEvt->getState()
            << std::endl << vprDEBUG_FLUSH;
        // x, y, z, state (modifier mask OR'd with button mask)*/
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
void KeyboardMouse::RegisterButtonSignals()
{
    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    int buttonOneIndex = gadget::MBUTTON1 - 1;

    for( int index = 1; index < 8; ++index )
    {
        std::stringstream ss;
        ss << index;
        std::string pressName("KeyboardMouse.ButtonPress");
        std::string releaseName("KeyboardMouse.ButtonRelease");
        pressName.append( ss.str() );
        releaseName.append( ss.str() );

        mButtonPressSignalHolder[ pressName ] = new ButtonPressSignal_type;
        mButtonReleaseSignalHolder[ releaseName ] = new ButtonReleaseSignal_type;

        mButtonPressSignalMap[ buttonOneIndex + index ] = 
            mButtonPressSignalHolder[ pressName ];
        mButtonReleaseSignalMap[ buttonOneIndex + index ] = 
            mButtonReleaseSignalHolder[ releaseName ];

        evm->RegisterSignal(
            new SignalWrapper< ButtonPressSignal_type >( 
                mButtonPressSignalHolder[ pressName ] ),
                pressName, eventmanager::EventManager::button_SignalType );

        evm->RegisterSignal(
            new SignalWrapper< ButtonReleaseSignal_type >( 
                mButtonReleaseSignalHolder[ releaseName ] ),
                releaseName, eventmanager::EventManager::button_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RegisterKeySignals()
{
    // We will use this to get access to gadget::KeyboardMouse::getKeyName
    mKeyboardMousePtr = gadget::KeyboardMousePtr( 
                                gadget::KeyboardMouse::create() );

    eventmanager::EventManager* evm = eventmanager::EventManager::instance();
    using eventmanager::SignalWrapper;

    // We will iterate through the values of gadget::Keys, adding a press and
    // release signal for each. The signals added will have names such as
    // KeyboardMouse.KeyPress_KEY_A   and  KeyboardMouse.KeyRelease_KEY_A.
    // We must do this in two blocks because the mouse buttons and some other
    // extraneous non-key stuff happens to be stuck right in the middle of
    // the gadget::Keys enum and must be avoided. This way of setting up
    // the key signals relies on two things:
    // 1) gadget::Keys doesn't get changed very frequently, so the block we
    //    avoid always sits in the same place
    // 2) gadget::KeyboardMouse provides a getKeyName method that can convert
    //    a gadget::Keys into a string.
    // If either of these requirements breaks, this key signal registering will
    // have to be adjusted.

    int key = gadget::KEY_NONE;

    while( key != gadget::MOUSE_POSX )
    {
        std::string pressName("KeyboardMouse.KeyPress_");
        std::string releaseName("KeyboardMouse.KeyRelease_");
        std::string keyName = mKeyboardMousePtr->getKeyName(
                static_cast< gadget::Keys >( key ) );
        pressName.append( keyName );
        releaseName.append( keyName );

        mKeyPressSignalHolder[ pressName ] = new KeyPressSignal_type;
        mKeyReleaseSignalHolder[ releaseName ] = new KeyReleaseSignal_type;

        mKeyPressSignalMap[ static_cast< gadget::Keys >( key ) ] =
                mKeyPressSignalHolder[ pressName ];
        mKeyReleaseSignalMap[ static_cast< gadget::Keys >( key ) ] =
                mKeyReleaseSignalHolder[ releaseName ];

        evm->RegisterSignal(
                new SignalWrapper< KeyPressSignal_type >(
                    mKeyPressSignalHolder[ pressName ] ),
                    pressName, 
                    eventmanager::EventManager::keyboard_SignalType );
        evm->RegisterSignal(
                new SignalWrapper< KeyReleaseSignal_type >(
                    mKeyReleaseSignalHolder[ releaseName ] ),
                    releaseName, 
                    eventmanager::EventManager::keyboard_SignalType );

        ++key;
    }

    key = gadget::KEY_TAB;
    while( key != gadget::KEY_UNKNOWN )
    {
        std::string pressName("KeyboardMouse.KeyPress_");
        std::string releaseName("KeyboardMouse.KeyRelease_");
        std::string keyName = mKeyboardMousePtr->getKeyName(
                static_cast< gadget::Keys >( key ) );
        pressName.append( keyName );
        releaseName.append( keyName );

        mKeyPressSignalHolder[ pressName ] = new KeyPressSignal_type;
        mKeyReleaseSignalHolder[ releaseName ] = new KeyReleaseSignal_type;

        mKeyPressSignalMap[ static_cast< gadget::Keys >( key ) ] =
                mKeyPressSignalHolder[ pressName ];
        mKeyReleaseSignalMap[ static_cast< gadget::Keys >( key ) ] =
                mKeyReleaseSignalHolder[ releaseName ];

        evm->RegisterSignal(
                new SignalWrapper< KeyPressSignal_type >(
                    mKeyPressSignalHolder[ pressName ] ),
                    pressName, 
                    eventmanager::EventManager::keyboard_SignalType );
        evm->RegisterSignal(
                new SignalWrapper< KeyReleaseSignal_type >(
                    mKeyReleaseSignalHolder[ releaseName ] ),
                    releaseName, 
                    eventmanager::EventManager::keyboard_SignalType );

        ++key;
    }
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
    if( !inputArea )
    {
        return vrj::DisplayPtr();
    }

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
void KeyboardMouse::SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint )
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
