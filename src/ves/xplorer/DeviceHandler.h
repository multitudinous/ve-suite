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

#ifndef VE_XPLORER_DEVICE_HANDLER
#define VE_XPLORER_DEVICE_HANDLER

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/DeviceHandlerPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/behavior/FrameAllPtr.h>
#include <ves/xplorer/behavior/CameraEventsPtr.h>
#include <ves/xplorer/behavior/CharacterNavigationPtr.h>
#include <ves/xplorer/behavior/ConstraintSelectionPtr.h>
#include <ves/xplorer/behavior/ManipulatorEventsPtr.h>
#include <ves/xplorer/behavior/NavigationPtr.h>
#include <ves/xplorer/behavior/SelectionPtr.h>

#include <switchwire/ScopedConnectionList.h>

// --- vrJuggler Includes --- //
#include <vpr/Util/Singleton.h>

#include <gmtl/Point.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/Event/AnalogEventInterface.h>
#include <gadget/Event/DigitalEventInterface.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/Quat>

// --- C/C++ Libraries --- //
#include <string>
#include <map>

namespace ves
{
namespace xplorer
{

namespace device
{
class Device;
class Wand;
class KeyboardMouse;
class GameController;
}

namespace event
{
class EventHandler;
}

/*!\file DeviceHandler.h
 * DeviceHandler API
 * \class ves::xplorer::DeviceHandler
 *
 */
class VE_XPLORER_EXPORTS DeviceHandler
{
private:
    ///Constructor
    DeviceHandler();

    ///Destructor
    ~DeviceHandler();

    ///
    ///\param DeviceHandler
    vprSingletonHeader( DeviceHandler );

public:
    ///Initialize all of the devices
    void Initialize();

    ///
    ///\param deviceName
    ///\param enable
    void EnableDevice( const device::Device::Type& type, const bool& enable );

    ///Execute navigation commands from active device
    void ExecuteCommands();

    ///Get the active coordinate system
    scenegraph::DCS* GetActiveDCS() const;

    ///Get a device
    ///\param deviceName The device name
    ///\return Get the device being requested
    device::Device* GetDevice( const device::Device::Type& type ) const;

    ///Get the device currently being processed
    ///Note: Will only return valid device during ProcessDeviceEvents() function
    ///\return Get the device being processed
    device::Device* GetDeviceBeingProcessed() const;

    ///Get the reset location of the world
    ///\param quat
    ///\param pos
    void GetResetWorldPosition( osg::Quat& quat, std::vector< double >& pos );

    ///Get the active coordinate system
    scenegraph::DCS* GetSelectedDCS() const;

    ///Process navigation and selection commands
    void ProcessDeviceEvents();

    ///Reset the center point of rotation
    void ResetCenterPoint();

    ///Set the active coordinate system
    ///\param dcs The current active coordinate system
    void SetActiveDCS( scenegraph::DCS* activeDCS );

    ///
    ///\param centerPoint
    void SetCenterPoint( gmtl::Point3d* centerPoint );

    ///Set the center point mode
    ///\param jumpMode Do not know what this does
    void SetCenterPointJumpMode( const std::string& jumpMode );

    ///Set the device mode
    ///\param deviceMode Do not know what this does
    void SetDeviceMode( const std::string& deviceMode );

    ///Set the reset location for the world
    ///This position is determined from the stored start position by the user
    ///\param quat
    ///\param pos
    void SetResetWorldPosition( osg::Quat& quat, std::vector< double >& pos );

    ///Set the selected dcs
    ///\param dcs The current selected dcs
    void SetSelectedDCS( scenegraph::DCS* selectedDCS );

    ///Unselect all currently selected objects
    void UnselectObjects();

    ///Get the group node that is holding the devices
    osg::Group* GetDeviceGroup();

protected:

private:
    ///Configure the game controller interfaces
    void ConfigureGameControllerDevices();

    ///Triggers a center point jump after this distance has been breached
    double mCenterPointThreshold;

    ///The distance the center point jumps along the +y axis
    double mCenterPointJump;

    ///The position of the reset location of the world
    std::vector< double > mResetPosition;

    ///The point about which rotation occurs
    gmtl::Point3d mCenterPoint;

    ///The reset position for the center point
    gmtl::Point3d mResetCenterPointPosition;

    ///The axis for the reset location of the world
    osg::Quat mResetAxis;

    ///The current device mode
    std::string mDeviceMode;

    ///A map of all the devices
    typedef std::map< const device::Device::Type, device::Device* > DeviceMap;
    DeviceMap m_deviceMap;

    ///A map of all the event handlers
    std::map< std::string, event::EventHandler* > mEventHandlers;

    ///The current active DCS
    osg::ref_ptr< scenegraph::DCS > mActiveDCS;

    ///The current selected DCS
    osg::ref_ptr< scenegraph::DCS > mSelectedDCS;

    ///
    device::Device* m_deviceBeingProcessed;

    ///The group node that holds all device graphical representations
    osg::ref_ptr< osg::Group > m_deviceGroup;

    ves::xplorer::behavior::FrameAllSharedPtr m_frameAllSlot;
    ves::xplorer::behavior::CameraEventsPtr m_cameraSlot;
    ves::xplorer::behavior::CharacterNavigationPtr m_characterNavSlot;
    ves::xplorer::behavior::ConstraintSelectionPtr m_constraintSlot;
    ves::xplorer::behavior::ManipulatorEventsPtr m_manipulatorSlot;
    ves::xplorer::behavior::NavigationPtr m_navSlot;
    ves::xplorer::behavior::SelectionPtr m_selectionSlot;

    switchwire::ScopedConnectionList m_connections;
    
    ///The enum to set the control state
    enum UserControlState
    {
        OpenControlState = (1u << 0),
        ClosedControlState = (1u << 1)
    };
    
    ///Determine whether a game controller can take over nav controlls
    UserControlState m_controlledState;
    
    enum GameControllerMask
    {
        GameController0 = 0,
        GameController1 = 1,
        GameController2 = 2,
        GameController3 = 3
    };

    GameControllerMask m_activeController;

    ///Try to grab controll
    void GrabControlState( GameControllerMask controllerMask );

    ///Check to see if we should open up the control state
    void CheckControlState();

    ///A vector holding the base names for gadgeteer analog and digital interfaces
    std::vector< std::string > m_gameControllerBaseNames;
    
    typedef gadget::AnalogEventInterface < gadget::event::all_events_tag,
        gadget::event::synchronized_tag > AnalogAxisInterface;
    
    typedef gadget::DigitalEventInterface < gadget::event::all_events_tag,
        gadget::event::synchronized_tag > GamePadClickInterface;
    struct GameControllerProxy
    {
        ///Track the position of the game controller so that we can tell what the
        ///current orientation of the controller is for updating the
        ///up and forward vectors of the controller.
        gadget::PositionInterface m_gamecontroller;
        
        typedef gadget::AnalogEventInterface < gadget::event::all_events_tag,
            gadget::event::synchronized_tag > AnalogAxisInterface;
        AnalogAxisInterface m_analogAxis0EventInterface;
        AnalogAxisInterface m_analogAxis1EventInterface;
        AnalogAxisInterface m_analogAxis2EventInterface;
        AnalogAxisInterface m_analogAxis3EventInterface;
        
        typedef gadget::DigitalEventInterface < gadget::event::all_events_tag,
            gadget::event::synchronized_tag > GamePadClickInterface;
        GamePadClickInterface m_button0EventInterface;
        GamePadClickInterface m_button1EventInterface;
        GamePadClickInterface m_button2EventInterface;
        GamePadClickInterface m_button3EventInterface;
        GamePadClickInterface m_button4EventInterface;
        GamePadClickInterface m_button5EventInterface;
        GamePadClickInterface m_button6EventInterface;
        GamePadClickInterface m_button7EventInterface;
        GamePadClickInterface m_button8EventInterface;
        GamePadClickInterface m_button9EventInterface;
        GamePadClickInterface m_button10EventInterface;
        GamePadClickInterface m_button11EventInterface;
        GamePadClickInterface m_button12EventInterface;
        GamePadClickInterface m_button13EventInterface;
        GamePadClickInterface m_button14EventInterface;
        
        ///The id of this controller which maps to the m_gameControllerBaseNames
        //list of names.
        GameControllerMask m_controllerMask;

        ///Call init on all of the event interfaces listed above
        ///\param deviceName The VR Juggler name given for the PositionInterface
        void InitInterfaces( const std::string deviceName );
        ///Connect the event interfaces to the GameController class callbacks
        ///\param controller The GameController pointer
        void ConnectInterfaces( device::GameController* const controller );
        ///Dicsonnect the event interfaces from the GameController class
        void DisconnectInterfaces();

        /// All GameController events get delivered here
        void OnAxis0Event( const float event );
        /// All GameController events get delivered here
        void OnAxis1Event( const float event );
        /// All GameController events get delivered here
        void OnAxis2Event( const float event );
        /// All GameController events get delivered here
        void OnAxis3Event( const float event );
        /// All GameController events get delivered here
        void OnAxis4Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnAxis5Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton0Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton2Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton4Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton5Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton6Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton7Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton10Event( gadget::DigitalState::State event );
        /// All GameController events get delivered here
        void OnButton11Event( gadget::DigitalState::State event );
    };
    
    ///A map holding gadgeteer device interfaces for game controllers
    std::map< std::string, GameControllerProxy* > m_gamControllerEvents;
};
} //end xplorer
} //end ves

#endif //VE_XPLORER_DEVICE_HANDLER
