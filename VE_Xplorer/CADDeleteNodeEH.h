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
#ifndef VE_CAD_DELETE_NODE_EVENT_HANDLER_H
#define VE_CAD_DELETE_NODE_EVENT_HANDLER_H
/*!\file CADDeleteNodeEventHandler.h
  CADDeleteNodeEventHandler API
  */
/*!\class CADDeleteNodeEventHandler
 * Class for removing CADNode from the current tree.
 */
namespace VE_XML
{
   class XMLObject;
}
namespace VE_CAD
{
   class CADNode;
}
#include "VE_Xplorer/CADEventHandler.h"
#include "VE_Installer/include/VEConfig.h"
namespace VE_EVENTS{
class VE_XPLORER_EXPORTS CADDeleteNodeEventHandler: public CADEventHandler{
public:
   ///Constructor
   CADDeleteNodeEventHandler();

   ///Copy Constructor
   CADDeleteNodeEventHandler(const CADDeleteNodeEventHandler& rhs);

   ///Destructor
   virtual ~CADDeleteNodeEventHandler();

   ///Equal operator
   CADDeleteNodeEventHandler& operator=(const CADDeleteNodeEventHandler& rhs);
protected:
   ///Remove a CADNode.
   ///\param command The Command containing the CADNode to remove.
   void _operateOnNode(VE_XML::XMLObject* command);
};
}
#endif// VE_EVENT_HANDLER_H
