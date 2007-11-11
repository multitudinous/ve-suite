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
#ifndef VE_CAD_ADD_NODE_EVENT_HANDLER_H
#define VE_CAD_ADD_NODE_EVENT_HANDLER_H

#include <ves/xplorer/event/cad/CADEventHandler.h>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CADAddNodeEventHandler.h
  CADTransfomrEventHandler API
  */
/*!\class CADAddNodeEventHandler
 * Class for handling CADNode transforms.
 */
class VE_XPLORER_EXPORTS CADAddNodeEventHandler: public CADEventHandler
{
public:
   ///Constructor
   CADAddNodeEventHandler();

   ///Copy Constructor
   CADAddNodeEventHandler(const CADAddNodeEventHandler& rhs);

   ///Destructor
   virtual ~CADAddNodeEventHandler();

   ///Equal operator
   CADAddNodeEventHandler& operator=(const CADAddNodeEventHandler& rhs);
protected:
   ///Update a transform on the CADNode.
   ///\param command The Command containing the udpated transform.
   void _operateOnNode(ves::open::xml::XMLObject* command);
};

}
}
}

#endif// VE_EVENT_HANDLER_H
