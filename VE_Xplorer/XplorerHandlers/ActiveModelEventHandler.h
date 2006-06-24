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
 * File:          $RCSfile: ActiveModelEventHandler.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ACTIVE_MODEL_EVENT_HANDLER_H
#define ACTIVE_MODEL_EVENT_HANDLER_H
/*!\file ActiveModelEventHandler.h
  ActiveModelEventHandler API
  */
/*!\class ActiveModelEventHandler
 * Class for handling the active model change.
 */

#include "VE_Xplorer/EventHandler.h"
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
class ActiveModelEventHandler : public EventHandler
{
public:
   ///Constructor
   ActiveModelEventHandler();

   ///Copy Constructor
   ActiveModelEventHandler(const ActiveModelEventHandler& ceh);

   ///Destructor
   virtual ~ActiveModelEventHandler();

   ///Set the cfdModel.
   ///\param model The cfdModelHandler to execute the Command on.
   void SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler );
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute(VE_XML::XMLObject* command); 

   ///Equal operator
   ActiveModelEventHandler& operator=(const ActiveModelEventHandler& rhs);
   
protected:
};
}
#endif// ACTIVE_MODEL_EVENT_HANDLER_H
