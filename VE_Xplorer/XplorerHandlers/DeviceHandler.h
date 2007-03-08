/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
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
#include "VE_Installer/include/VEConfig.h"

#include <vpr/Util/Singleton.h>

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
public:
	DeviceHandler();
   ~DeviceHandler(){;}
   vprSingletonHeader( DeviceHandler );

   void CleanUp();
   void ExecuteCommands();

   void SetActiveDevice( std::string device );
   void SetDeviceMode( unsigned int mode );
   void ProcessDeviceEvents();

   VE_Xplorer::Wand* GetWand();
   VE_Xplorer::KeyboardMouse* GetKeyboardMouse();
   
private:
   std::map< std::string, VE_Xplorer::Device* > devices;
   std::map< std::string, VE_EVENTS::EventHandler* > _eventHandlers;

   VE_Xplorer::Device* active_device;
   bool navigation;

};
}

#endif //DEVICE_HANDLER_H
