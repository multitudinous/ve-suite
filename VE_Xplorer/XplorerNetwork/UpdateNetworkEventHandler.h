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
 * Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8206 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef UPDATE_NETWORK_EVENT_HANDLER_H
#define UPDATE_NETWORK_EVENT_HANDLER_H
/*!\file UpdateNetworkEventHandler.h
  UpdateNetworkEventHandler API
  */
/*!\class UpdateNetworkEventHandler
 * Reload plugins for the network.
 */

#include "VE_Installer/include/VEConfig.h"

namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdGlobalBase;
   class cfdVEBaseClass;
}
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
namespace VE_EVENTS
{
class VE_XPLORER_NETWORK_EXPORTS UpdateNetworkEventHandler: public EventHandler
{
public:
   ///Constructor
   UpdateNetworkEventHandler();

   ///Copy Constructor
   UpdateNetworkEventHandler(const UpdateNetworkEventHandler& rhs);

   ///Destructor
   virtual ~UpdateNetworkEventHandler();

   ///Equal operator
   UpdateNetworkEventHandler& operator=(const UpdateNetworkEventHandler& rhs);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model=0);
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute( VE_XML::XMLObject* command); 

private:
};
}
#endif// RELOAD_PLUGINS_EVENT_HANDLER_H
