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
#ifndef NAVIGATION_EVENT_HANDLER_H
#define NAVIGATION_EVENT_HANDLER_H
/*!\file NavigationDataEventHandler.h
  NavigationDataEventHandler API
  */
/*!\class NavigationDataEventHandler
 * Class for changing trackball properties in xplorer
 */
#include <ves/xplorer/event/EventHandler.h>

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

namespace ves
{
namespace xplorer
{
namespace event
{
class NavigationDataEventHandler: public EventHandler
{
public:
   //Constructor
   NavigationDataEventHandler();

   //Copy Constructor
   NavigationDataEventHandler(const NavigationDataEventHandler& ceh);

   //Destructor
   virtual ~NavigationDataEventHandler();

   //Set the cfdModel
   //param model The cfdModelHandler to execute the Command on
   void SetGlobalBaseObject(ves::xplorer::cfdGlobalBase* modelHandler);

   //Exectute the event
   //param xmlObject The current xmlObject event.
   void Execute(ves::open::xml::XMLObject* command); 

   //Equal operator
   NavigationDataEventHandler& operator=(const NavigationDataEventHandler& rhs);

protected:
};

}
}
}

#endif//NAVIGATION_EVENT_HANDLER_H
