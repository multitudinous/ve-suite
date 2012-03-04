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
#include <ves/xplorer/behavior/CharacterNavigation.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

//OSG
#include <osg/BoundingSphere>
#include <osg/Vec3d>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>

//GMTL
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{
////////////////////////////////////////////////////////////////////////////////
CharacterNavigation::CharacterNavigation()
    :
    m_physicsSimulator( *ves::xplorer::scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *ves::xplorer::scenegraph::SceneManager::instance() ),
    m_characterController( m_sceneManager.GetCharacterController() ),
    m_manipulatorManager( m_sceneManager.GetManipulatorManager() ),
    m_cameraManager( m_sceneManager.GetCameraManager() ),
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_currX( 0 ),
    m_currY( 0 ),
    m_pickedBody( 0 ),
    m_pickConstraint( 0 )
{    
    //Setup connection to key press events
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_A", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_S", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_W", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_D", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_X", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_SPACE", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyPressEvent,
        m_connections, any_SignalType, normal_Priority );

    //Setup connection to key release events
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_A", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_S", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_W", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_D", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_X", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyRelease_KEY_SPACE", bool(gadget::Keys, int, char), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessKeyReleaseEvent,
        m_connections, any_SignalType, normal_Priority );
                     
    //Setup connection to mouse events
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.MouseMove", bool( int, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseMove,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_5_COMBINER( "KeyboardMouse.Scroll", bool( int, int, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseScroll,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress1%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMousePress,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress2%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMousePress,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress3%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMousePress,
        m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress4%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMousePress,
        m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress5%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMousePress,
        m_connections, any_SignalType, normal_Priority );    
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease1%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseRelease,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease2%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseRelease,
        m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease3%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseRelease,
        m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease4%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseRelease,
        m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease5%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CharacterNavigation::ProcessMouseRelease,
        m_connections, any_SignalType, normal_Priority );    
}
////////////////////////////////////////////////////////////////////////////////
CharacterNavigation::~CharacterNavigation()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessKeyPressEvent( gadget::Keys buttonKey, int modifierState, char keyChar )
{ 
    if( modifierState != gadget::KEY_NONE )
    {
        return false;
    }
    boost::ignore_unused_variable_warning( keyChar );

    switch( buttonKey )
    {
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( true );
            return true;
        }
        
        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( true );
            return true;
        }
        
        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( true );
            return true;
        }
        
        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( true );
            return true;
        }
        
        break;
    }
    case gadget::KEY_X:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.IsFlying() )
            {
                m_characterController.StepDown( true );
            }
        }
        
        break;
    }
    case gadget::KEY_SPACE:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.IsFlying() )
            {
                m_characterController.StepUp( true );
            }
            else
            {
                //m_characterController.Jump();
            }
        }
        
        break;
    }
    default:
    {
        return false;
    }
    } //end switch( m_currKey )
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessKeyReleaseEvent( gadget::Keys buttonKey, int modifierState, char keyChar )
{
    if( modifierState != gadget::KEY_NONE )
    {
        return false;
    }
    boost::ignore_unused_variable_warning( keyChar );

    switch( buttonKey )
    {
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( false );
            return true;
        }
        
        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( false );
            return true;
        }
        
        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( false );
            return true;
        }
        
        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( false );
            return true;
        }
        
        break;
    }
    case gadget::KEY_X:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.IsFlying() )
            {
                m_characterController.StepDown( false );
            }
            else
            {
                ;
            }
        }
        
        break;
    }
    case gadget::KEY_SPACE:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.IsFlying() )
            {
                m_characterController.StepUp( false );
            }
            else
            {
                ;
            }
        }
        
        break;
    }
    default:
    {
        break;
    }
    } //end switch( m_currKey )
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessMousePress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{        
    m_currX = xPos;
    m_currY = yPos;
    m_prevX = m_currX;
    m_prevY = m_currY;

    switch( buttonKey )
    {
    //Left mouse button
    case gadget::MBUTTON1:
    {
        //Rotate just the camera "3rd person view:
        if( m_characterController.IsEnabled() )
        {
            m_characterController.FirstPersonMode( false );
        }
        
        //No modifier key
        if( !(buttonState&(gadget::SHIFT_MASK|gadget::CTRL_MASK|gadget::ALT_MASK)) )
        {
            if( m_characterController.IsEnabled() )
            {
                m_characterController.SetCameraRotationSLERP( false );
            }
        }
        
        break;
    }
    //Middle mouse button
    case gadget::MBUTTON2:
    {                
        break;
    }
    //Right mouse button
    case gadget::MBUTTON3:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.FirstPersonMode( true );
            m_characterController.SetCameraRotationSLERP( false );
            m_characterController.SetRotationFromCamera();
        }

        break;
    }
    default:
    {
        ;
    }
    } //end switch( m_currKey )
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessMouseRelease( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    boost::ignore_unused_variable_warning( xPos );
    boost::ignore_unused_variable_warning( yPos );
    boost::ignore_unused_variable_warning( buttonState );

    switch( buttonKey )
    {
    //Left mouse button
    case gadget::MBUTTON1:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.SetCameraRotationSLERP( true );
        }
        break;
    }
    //Middle mouse button
    case gadget::MBUTTON2:
    {
        break;
    }
    //Right mouse button
    case gadget::MBUTTON3:
    {
        break;
    }
    default:
    {
        return false;
    }
    } //end switch( m_currKey )
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessMouseMove( int xPos, int yPos, int zPos, int buttonState )
{
    boost::ignore_unused_variable_warning( zPos );
    
    if( buttonState == 0 )
    {
        return false;
    }

    m_currX = xPos;
    m_currY = yPos;
    double dx = m_currX - m_prevX;
    double dy = m_currY - m_prevY;
    m_prevX = m_currX;
    m_prevY = m_currY;

    const int& windowWidth = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowWidth();
    const int& windowHeight = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowHeight();
    
    dx /= windowWidth;
    dy /= windowHeight;
    //Left mouse button
    if( buttonState&(gadget::BUTTON1_MASK) )
    {
        //I do not know how to test for no modifier keys here...
        //No modifier key
        if( !(buttonState&(gadget::SHIFT_MASK|gadget::CTRL_MASK|gadget::ALT_MASK)) )
        {
            //Rotate just the camera "3rd person view:
            if( m_characterController.IsEnabled() )
            {
                m_characterController.Rotate( dx, dy );
            }
        }

        return false;
    }
    //Middle mouse button
    if( buttonState&gadget::BUTTON2_MASK )
    {
        return false;
    }
    //Right mouse button
    if( buttonState&gadget::BUTTON3_MASK )
    {
        //Rotate the character and camera at the same time
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Rotate( dx, dy );
        }

        return false;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterNavigation::ProcessMouseScroll( int deltaX, int deltaY, int x, int y, int buttonState )
{
    boost::ignore_unused_variable_warning( deltaX );
    boost::ignore_unused_variable_warning( x );
    boost::ignore_unused_variable_warning( y );
    boost::ignore_unused_variable_warning( buttonState );

    //Scroll wheel up
    //case gadget::MBUTTON4:
    if( deltaY > 0 )
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( true );
            return true;
        }
    }
    //Scroll wheel down
    //if( buttonState&gadget::MBUTTON5 )
    else if( deltaY < 0 )
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( false );
            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
