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
#ifndef VES_XPLORER_DEVICE_GAMECONTROLLER_H
#define VES_XPLORER_DEVICE_GAMECONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/xplorer/device/Device.h>

#include <switchwire/BooleanPropagationCombiner.h>

// --- vrJuggler Includes --- //

#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <boost/shared_ptr.hpp>

#include <gadget/gadgetParam.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/Event/AnalogEventInterface.h>
#include <gadget/Event/DigitalEventInterface.h>

#include <gadget/Event/EventPtr.h>

#include <ves/xplorer/device/GameController.h>

namespace gadget
{
class InputArea;
}

namespace osgwMx
{
class MxGamePad;
class FunctionalMap;
class MxCore;
}

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- Boost includes --- //
#include <switchwire/Event.h>

// --- STL Includes --- //
#include <bitset>

#include <ves/util/GNUCompilerGuards.h>
DIAG_OFF(unused-parameter)
#include <boost/date_time/posix_time/posix_time.hpp>
DIAG_ON(unused-parameter)

namespace ves
{
namespace xplorer
{
namespace device
{

/*!\file GameControllerCallbacks.h
 * \class ves::xplorer::device::GameControllerCallbacks
 * \namespace ves::xplorer::device
 *
 */
class VE_XPLORER_EXPORTS GameControllerCallbacks : public Device
{
public:
    enum AnalogControlMode { NAV = 0x1, UI = 0x2, USER_DEFINED = 0x4 };

    ///Constructor
    GameControllerCallbacks();

    ///Destructor
    virtual ~GameControllerCallbacks();

    ///
    ///\return
    virtual GameControllerCallbacks* AsGameController();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Check to see if we should open up the control state
    void CheckControlState();

private:
    ///Configure the game controller interfaces
    void ConfigureGameControllerDevices();
    
    ///Update the forward and up vector for the game controller
    void UpdateForwardAndUp();// const osg::Vec3d&, const osg::Vec3d& );

    ///The enum to set the control state
    enum UserControlState
    {
        OpenControlState = (1u << 0),
        ClosedControlState = (1u << 1)
    };
    
    ///Determine whether a game controller can take over nav controlls
    UserControlState m_controlledState;
    
    unsigned int m_activeController;
    
    ///Try to grab controll
    void GrabControlState( unsigned int const& controllerMask );
        
    ///A vector holding the base names for gadgeteer analog and digital interfaces
    std::vector< std::string > m_gameControllerBaseNames;

    /*gadget::RumbleInterface _rumble;
    gadget::HatInterface _hats[1];
	gadget::RumbleEffectPtr _speed;
	gadget::RumbleEffectPtr _objectHit[4];
    gadget::HatState::State _oldHatState;*/

    
    ///A map holding gadgeteer device interfaces for game controllers
    std::map< std::string, GameController* > m_gamControllerEvents;
    
    ///The start of the controller ownership
    boost::posix_time::ptime m_lastActiveTime;

public:
    ///Callback for the position data from the controller
    ///\note Proxy name VESJoystick0Position, VESJoystick1Position, VESJoystick2Position, VESJoystick3Position
    void OnPositionEvent( gmtl::Matrix44f mat );

    /// All GameController events get delivered here
    ///\note This is the left stick
    void OnAxis0Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the left stick
    void OnAxis1Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the right stick
    void OnAxis2Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the right stick
    void OnAxis3Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the trigger
    void OnAxis4Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the trigger
    void OnAxis5Event( const float event );

    /// All GameController events get delivered here
    void OnButton0Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton1Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton2Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton3Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton4Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton5Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton6Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton7Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton8Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton9Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton10Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton11Event( gadget::DigitalState::State event );

    void OnHat0Event( gadget::HatState::State event );
private:
    ///Setup the start and end point for a mouse pick
    void SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );

    ///Set the character state to enable controlling the nav step size
    void SetCharacterState( bool const& enable );

    ///The current X mouse position
    int m_currX;

    ///The current Y mouse position
    int m_currY;

    ///Determine if the mouse is inside the UI
    bool m_exit;

    ///Selection ray start point
    osg::Vec3d m_startPoint;
    ///Selection ray end point
    osg::Vec3d m_endPoint;

    ///THe position of the controller
    gmtl::Matrix44f m_controllerPosition;

    /// signal for generating the start and end point for selection and other
    ///interaction tools.
    /// Params are: start point and end point
    typedef switchwire::Event< void ( osg::Vec3d, osg::Vec3d ) > StartEndPointSignal_type;
    StartEndPointSignal_type m_startEndPointSignal;

    /// MouseMove signal
    /// Params are: x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal < bool ( int, int, int, int ),
            switchwire::BooleanPropagationCombiner > MouseMoveSignal_type;
    MouseMoveSignal_type m_mouseMove;

    ///Hide show ui signal type
    ves::util::VoidSignal_type m_hideShowUI;

    ///Slot to control the nav mode
    void SetRotationMode( std::string rotationMode );

    /// Scroll signal type
    /// Params are: deltaX, deltaY, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal < bool ( int, int, int, int, int ),
            switchwire::BooleanPropagationCombiner > ScrollSignal_type;
    ScrollSignal_type m_scroll;

    //osg::ref_ptr< osgwMx::MxInputAdapterGadgeteerGamePad > m_gadgetInputAdapter;
    osg::ref_ptr< osgwMx::MxGamePad > m_mxGamePadStyle;

    float m_leftStickX;
    float m_leftStickY;
    float m_rightStickX;
    float m_rightStickY;

    ///Button enum container
    unsigned int m_buttons;
    ///Hold the mapping from buttons to functions
    osg::ref_ptr< osgwMx::FunctionalMap > m_buttonMap;
    ///MxCore pointer
    osgwMx::MxCore& m_viewMatrix;
    ///Nav mode wether orbit or view
    std::string m_navMode;
    ///The signature to tell others the game pad is active
    ves::util::BoolSignal_type m_updateData;
    ///Control the data flow
    bool m_success;
    ///If the analog sticks are being used for nav, to move the cursor, or some script-defined behavior
    AnalogControlMode m_analogControlMode;

    // "raw" button signal type
    // this signal should only be used with Squirrel code; C++ slots should not be doing any conditional
    // processing based on the button state; instead, subscribe C++ slots to a state-specific "void" signal
    // (and create such signals if they are needed)
    typedef switchwire::Event< void ( gadget::DigitalState::State ) > GameControllerButtonSignal_type;

    // "raw" analog signal type
    typedef switchwire::Event< void ( float ) > GameControllerAnalogSignal_type;

    // "raw" hat signal type
    typedef switchwire::Event< void ( gadget::HatState::State ) > GameControllerHatSignal_type;

    typedef std::map< std::string, GameControllerButtonSignal_type* > GameControllerButtonSignalMap_type;
    typedef std::map< std::string, GameControllerAnalogSignal_type* > GameControllerAnalogSignalMap_type;

    GameControllerButtonSignalMap_type m_gameControllerButtonSignalMap;
    GameControllerAnalogSignalMap_type m_gameControllerAnalogSignalMap;
    GameControllerHatSignal_type m_gameControllerHatSignal;
};

} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_GAMECONTROLLER_H
