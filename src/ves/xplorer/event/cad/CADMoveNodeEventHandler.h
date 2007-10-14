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
#ifndef VE_CAD_MOVE_NODE_EVENT_HANDLER_H
#define VE_CAD_MOVE_NODE_EVENT_HANDLER_H
/*!\file CADMoveNodeEventHandler.h
  CADMoveNodeEventHandler API
  */
/*!\class CADMoveNodeEventHandler
 * Class that handles moving a CADNode to a different CADAssembly.
 */
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

#include <ves/xplorer/event/cad/CADEventHandler.h>
#include <ves/VEConfig.h>
namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS CADMoveNodeEventHandler: public CADEventHandler{
public:
   ///Constructor
   CADMoveNodeEventHandler();

   ///Copy Constructor
   CADMoveNodeEventHandler(const CADMoveNodeEventHandler& rhs);

   ///Destructor
   virtual ~CADMoveNodeEventHandler();

   ///Equal operator
   CADMoveNodeEventHandler& operator=(const CADMoveNodeEventHandler& rhs);
protected:
   ///Update a transform on the CADNode.
   ///\param command The Command containing the udpated transform.
   void _operateOnNode(ves::open::xml::XMLObject* command);
};
}
#endif// VE_CAD_MOVE_NODE_EVENT_HANDLER_H
