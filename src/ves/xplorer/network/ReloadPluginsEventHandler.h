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
#ifndef RELOAD_PLUGINS_EVENT_HANDLER_H
#define RELOAD_PLUGINS_EVENT_HANDLER_H
/*!\file ReloadPluginsEventHandler.h
  ReloadPluginsEventHandler API
  */
/*!\class ReloadPluginsEventHandler
 * Reload plugins for the network.
 */

#include <ves/VEConfig.h>

namespace ves
{
namespace open
{
namespace xml
{
    class XMLObject;
}
}
}

namespace ves
{
namespace xplorer
{
   class cfdGlobalBase;
}
}

#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS ReloadPluginsEventHandler: public ves::xplorer::event::EventHandler
{
public:
   ///Constructor
   ReloadPluginsEventHandler();

   ///Copy Constructor
   ReloadPluginsEventHandler(const ReloadPluginsEventHandler& rhs);

   ///Destructor
   virtual ~ReloadPluginsEventHandler();

   ///Equal operator
   ReloadPluginsEventHandler& operator=(const ReloadPluginsEventHandler& rhs);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(ves::xplorer::cfdGlobalBase* model=0);
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute( ves::open::xml::XMLObject* command); 

private:
};
}
}
}
#endif// RELOAD_PLUGINS_EVENT_HANDLER_H
