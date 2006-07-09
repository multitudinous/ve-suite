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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CHANGE_CURSOR_EVENT_HANDLER_H
#define CHANGE_CURSOR_EVENT_HANDLER_H
/*!\file ChangeCursorEventHandler.h
  ChangeCursorEventHandler API
  */
/*!\class ChangeCursorEventHandler
 * Class for deleting objects from network.
 */
//#include <string>
//#include <vector>
#include <map>
#include "VE_Installer/include/VEConfig.h"
//#include <utility>

namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdModel;
   class cfdGlobalBase;
   class cfdVEBaseClass;
}
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS ChangeCursorEventHandler: public EventHandler
{
public:
   ///Constructor
   ChangeCursorEventHandler();

   ///Copy Constructor
   ChangeCursorEventHandler(const ChangeCursorEventHandler& rhs);

   ///Destructor
   virtual ~ChangeCursorEventHandler();

   ///Equal operator
   ChangeCursorEventHandler& operator=(const ChangeCursorEventHandler& rhs);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model=0);
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute( VE_XML::XMLObject* command); 

private:
   VE_Xplorer::cfdModel* _activeModel;
};
}
#endif// CHANGE_CURSOR_EVENT_HANDLER_H
