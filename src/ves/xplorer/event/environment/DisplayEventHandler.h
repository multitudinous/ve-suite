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
#ifndef DISPLAY_EVENT_HANDLER_H
#define DISPLAY_EVENT_HANDLER_H
/*!\file DisplayEventHandler.h
  DisplayEventHandler API
  */
/*!\class DisplayEventHandler
 * Class for changing trackball properties in xplorer
 */
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"

namespace VE_XML
{
   class XMLObject;
}

namespace VE_Xplorer
{
   class cfdGlobalBase;
}

namespace VE_EVENTS
{
   class DisplayEventHandler:public EventHandler
   {
      public:
         //Constructor
         DisplayEventHandler();

         //Copy Constructor
         DisplayEventHandler(const DisplayEventHandler& ceh);

         //Destructor
         virtual ~DisplayEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         DisplayEventHandler& operator=(const DisplayEventHandler& rhs);
   
      private:

   };
}

#endif//DISPLAY_EVENT_HANDLER_H
