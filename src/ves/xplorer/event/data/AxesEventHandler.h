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
#ifndef AXES_EVENT_HANDLER_H
#define AXES_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/ModelPtr.h>

#include <map>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file AxesEventHandler.h
  AxesEventHandler API
  */
/*!\class AxesEventHandler
 * 
 */
class VE_XPLORER_EXPORTS AxesEventHandler: public EventHandler
{
public:
   ///Constructor
   AxesEventHandler();

   ///Copy Constructor
   AxesEventHandler(const AxesEventHandler& rhs);

   ///Destructor
   virtual ~AxesEventHandler();

   ///Equal operator
   AxesEventHandler& operator=(const AxesEventHandler& rhs);

   ///Set the Model.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(ves::xplorer::GlobalBase* model=0);
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute( ves::open::xml::XMLObject* command); 

private:
   ves::xplorer::Model* _activeModel;///<The active cfdModel
};

}
}
}

#endif// AXES_EVENT_HANDLER_H
