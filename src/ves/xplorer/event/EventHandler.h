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
#ifndef VE_EVENT_HANDLER_H
#define VE_EVENT_HANDLER_H
/*!\file EventHandler.h
  EventHandler API
  */
/*!\class EventHandler
 * Base class for event handling.
 */
/*!\namespace VE_EVENTS
 * Namespace for ve-event handlers.
 */
#include <ves/open/xml/XMLObjectPtr.h>
namespace VE_Xplorer
{
   class cfdGlobalBase;
}
#include <ves/VEConfig.h>

namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS EventHandler
{
public:
   ///Constructor
   EventHandler(){;}

   ///Destructor
   virtual ~EventHandler(){;}

   ///The call to handle the event
   ///\param objectToProcess The xml Object to process
   virtual void Execute(ves::open::xml::XMLObject* objectToProcess=0) = 0;

   ///\param baseObject The cfdGlobalBase object to apply the command to.
   virtual void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject=0) = 0;
protected:
   ///<the variable of the global base object to operate on
   VE_Xplorer::cfdGlobalBase* _baseObject;
};
}
#endif// VE_EVENT_HANDLER_H
