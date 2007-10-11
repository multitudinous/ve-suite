/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DEVICE_HANDLER
#define DEVICE_HANDLER
/*!\file DeviceHandler.h
DeviceHandler API
*/
/*!\class VE_Xplorer::DeviceHandler
* 
*/
// --- VE-Suite Stuff --- //
#include <ves/VEConfig.h>

#include <VE_Xplorer/SceneGraph/DCS.h>

// --- VR Juggler Stuff --- //
#include <vpr/Util/Singleton.h>

#include <gmtl/Point.h>

//C/C++ Libraries
#include <string>
#include <map>

namespace VE_Xplorer
{
    class Device;
    class Wand;
    class KeyboardMouse;
}

namespace VE_EVENTS
{
    class EventHandler;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS DeviceHandler
{
private:
    ///Constructor
    DeviceHandler();

    ///Destructor
    ~DeviceHandler();

    ///Do not know what this is
    ///\param DeviceHandler 
    vprSingletonHeader( DeviceHandler );
public:
    ///Delete existing devices
    //void CleanUp();

    ///Execute navigation commands from active device
    void ExecuteCommands();

    ///Set the active device
    ///\param device The active device
    void SetActiveDevice( std::string device );

    ///Set the device mode
    ///\param mode Do not know what this does
    void SetDeviceMode( std::string mode );

    ///Set the center point mode
    ///\param mode Do not know what this does
    void SetCenterPointJumpMode( std::string mode );

    ///Unselect all currently selected objects
    void UnselectObjects();

    ///Process navigation and selection commands
    void ProcessDeviceEvents();

    ///Get a device
    ///\param device The device
    ///\return Get the device being requested
    VE_Xplorer::Device* GetDevice( std::string device );
    
    ///Get active device
    ///\return Get the active device
    VE_Xplorer::Device* GetActiveDevice();

private:
    std::map< std::string, VE_Xplorer::Device* > devices; ///<
    std::map< std::string, VE_EVENTS::EventHandler* > _eventHandlers;

    VE_Xplorer::Device* active_device; ///<The active device
    osg::ref_ptr< VE_SceneGraph::DCS > m_activeDCS;///<The active coordinate system
    osg::ref_ptr< VE_SceneGraph::DCS > selectedDCS;///<The selected coordinate system
    std::string device_mode; ///<Tells whether navigation or selection is active
    gmtl::Point3d center_point; ///<Do not know what this is
    double m_threshold;///<
    double m_jump;///<
};
}

#endif //DEVICE_HANDLER_H
