/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

// --- vrJuggler Includes --- //
#include <vpr/Util/Singleton.h>

#include <gmtl/Point.h>

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
}

namespace event
{
class EventHandler;
}

/*!\file DeviceHandler.h
 * DeviceHandler API
 */

/*!\class ves::xplorer::DeviceHandler
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
    ///Execute navigation commands from active device
    void ExecuteCommands();

    ///Get the active coordinate system
    ves::xplorer::scenegraph::DCS* const GetActiveDCS() const;

    ///Get active device
    ///\return Get the active device
    ves::xplorer::device::Device* const GetActiveDevice() const;

    ///Get a device
    ///\param deviceName The device name
    ///\return Get the device being requested
    ves::xplorer::device::Device* const GetDevice(
        const std::string& deviceName ) const;

    ///Get the reset location of the world
    ///\param quat
    ///\param pos
    void GetResetWorldPosition( osg::Quat& quat, std::vector< double >& pos );

    ///Get the active coordinate system
    ves::xplorer::scenegraph::DCS* const GetSelectedDCS() const;

    ///Process navigation and selection commands
    void ProcessDeviceEvents();

    ///Reset the center point of rotation
    void ResetCenterPoint();

    ///Set the active coordinate system
    ///\param dcs The current active coordinate system
    void SetActiveDCS( ves::xplorer::scenegraph::DCS* activeDCS );

    ///Set the active device
    ///\param activeDevice The active device
    void SetActiveDevice( const std::string& activeDevice );

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
    void SetSelectedDCS( ves::xplorer::scenegraph::DCS* selectedDCS );

    ///Unselect all currently selected objects
    void UnselectObjects();

    ///Set RTT mode
    void SetRTTMode( bool rttMode );
    
    ///Get RTT mode
    bool GetRTTMode();
    
protected:

private:
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

    ///The current active device
    ves::xplorer::device::Device* mActiveDevice;

    ///A map of all the devices
    std::map< const std::string, ves::xplorer::device::Device* > mDevices;

    ///A map of all the event handlers
    std::map< std::string, ves::xplorer::event::EventHandler* > mEventHandlers;

    ///The current active DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mActiveDCS;

    ///The current selected DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mSelectedDCS;
    
    ///Tablet convenience device pointer
    ves::xplorer::device::Device* mTabletDevice;

    ///Tablet convenience device pointer
    ves::xplorer::device::Device* mGlovesDevice;

    ///Tablet convenience device pointer
    ves::xplorer::device::Device* mWandDevice;

    ///Tablet convenience device pointer
    ves::xplorer::device::Device* mKMDevice;

    /// rtt mode
    bool m_rttMode;
};
} //end xplorer
} //end ves

#endif //VE_XPLORER_DEVICE_HANDLER
