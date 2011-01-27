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
#include <ves/xplorer/behavior/CharacterNavigation.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>

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
    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_A", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_S", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_W", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_D", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_X", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3( "KeyboardMouse.KeyPress_KEY_SPACE", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );

    //Setup connection to key release events
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_A", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_S", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_W", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_D", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_X", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_3( "KeyboardMouse.KeyRelease_KEY_SPACE", void(gadget::Keys, int, char), &CharacterNavigation::ProcessKeyPressEvent,
                     m_connections, any_SignalType, normal_Priority );
                     
    //Setup connection to mouse events
    CONNECTSIGNALS_4( "KeyboardMouse.MouseMove", void( int, int, int, int ), &CharacterNavigation::ProcessMouseMove,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "KeyboardMouse.ButtonPress1%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMousePress,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "KeyboardMouse.ButtonPress2%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMousePress,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "KeyboardMouse.ButtonPress3%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMousePress,
                     m_connections, any_SignalType, normal_Priority );    

    CONNECTSIGNALS_4( "KeyboardMouse.ButtonRelease1%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMouseRelease,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "KeyboardMouse.ButtonRelease2%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMouseRelease,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "KeyboardMouse.ButtonRelease3%", void( gadget::Keys, int, int, int ), &CharacterNavigation::ProcessMouseRelease,
                     m_connections, any_SignalType, normal_Priority );    
}
////////////////////////////////////////////////////////////////////////////////
CharacterNavigation::~CharacterNavigation()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterNavigation::ProcessKeyPressEvent( gadget::Keys buttonKey, int modifierState, char keyChar )
{    
    switch( buttonKey )
    {
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( true );
        }
        
        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( true );
        }
        
        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( true );
        }
        
        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( true );
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
        break;
    }
    } //end switch( m_currKey )
}
////////////////////////////////////////////////////////////////////////////////
void CharacterNavigation::ProcessKeyReleaseEvent( gadget::Keys buttonKey, int modifierState, char keyChar )
{
    switch( buttonKey )
    {
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( false );
        }
        
        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( false );
        }
        
        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( false );
        }
        
        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( false );
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
}
////////////////////////////////////////////////////////////////////////////////
void CharacterNavigation::ProcessMousePress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{        
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
        if( buttonState == 0 )
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
    //Scroll wheel up
    case gadget::MBUTTON4:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( true );
        }
        
        break;
    }
    //Scroll wheel down
    if( buttonState&gadget::MBUTTON5 )
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( false );
        }
        
        return;
    }
    default:
    {
        ;
    }
    } //end switch( m_currKey )
}
////////////////////////////////////////////////////////////////////////////////
void CharacterNavigation::ProcessMouseRelease( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
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
        ;
    }
    } //end switch( m_currKey )
}
////////////////////////////////////////////////////////////////////////////////
void CharacterNavigation::ProcessMouseMove( int xPos, int yPos, int zPos, int buttonState )
{
    if( buttonState == 0 )
    {
        return;
    }

    int dx = 0;
    int dy = 0;
    const int& windowWidth = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowWidth();
    const int& windowHeight = m_sceneManager.GetCurrentGLTransformInfo()->GetWindowHeight();
    
    dx /= windowWidth;
    dy /= windowHeight;
    
    //Left mouse button
    if( buttonState&(gadget::BUTTON1_MASK|0) )
    {
        //No modifier key
        //if( mKeyNone )
        {
            //Rotate just the camera "3rd person view:
            if( m_characterController.IsEnabled() )
            {
                m_characterController.Rotate( dx, dy );
            }
        }

        return;
    }
    //Middle mouse button
    if( buttonState&gadget::BUTTON2_MASK )
    {
        return;
    }
    //Right mouse button
    if( buttonState&gadget::BUTTON3_MASK )
    {
        //Rotate the character and camera at the same time
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Rotate( dx, dy );
        }

        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
