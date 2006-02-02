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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_EVENT_HANDLER_H
#define CAD_EVENT_HANDLER_H
/*!\file CADEventHandler.h
  CADEventHandler API
  */
/*!\class CADEventHandler
 * Base class for CADNode event handling.
 */

#include "VE_Xplorer/EventHandler.h"
namespace VE_CAD
{
   class CADNode;
}
namespace VE_Xplorer
{
   class cfdGlobalBase;
}
namespace VE_EVENTS{
class CADEventHandler : public EventHandler{
public:
   ///Constructor
   CADEventHandler();

   ///Copy Constructor
   CADEventHandler(const CADEventHandler& ceh);

   ///Destructor
   virtual ~CADEventHandler();

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model);
   
   ///Exectute the event
   ///\param command The current Command event.
   void Execute(VE_XML::Command* command); 

   ///Equal operator
   CADEventHandler& operator=(const CADEventHandler& rhs);
   
protected:
   ///The internal operation on the CADNode.
   ///\param command The Command to execute.
   virtual void _operateOnNode(VE_XML::Command* command) = 0;

   VE_CAD::CADNode* _cadNode;///<The CADNode.
};
}
#endif// VE_EVENT_HANDLER_H
