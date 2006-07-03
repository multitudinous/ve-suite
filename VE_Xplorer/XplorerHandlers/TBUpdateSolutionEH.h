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
 * Date modified: $Date: 2006-06-24 17:15:54 -0500 (Sat, 24 Jun 2006) $
 * Version:       $Rev: 4730 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXTURE_BASED_UPDATE_SOLUTION_EVENT_HANDLER_H
#define TEXTURE_BASED_UPDATE_SOLUTION_EVENT_HANDLER_H
/*!\file TBActivateEH.h
  TextureBasedUpdateSolutionEventHandler API
  */
/*!\class TextureBasedUpdateSolutionEventHandler
 * Activate the texture based visualization.
 */

#include "VE_Xplorer/XplorerHandlers/TextureBasedEventHandler.h"
namespace VE_XML
{
   class XMLObject;
}
#include "VE_Installer/include/VEConfig.h"

namespace VE_EVENTS{
class VE_XPLORER_EXPORTS TextureBasedUpdateSolutionEventHandler : public TextureBasedEventHandler{
public:
   ///Constructor
   TextureBasedUpdateSolutionEventHandler();

   ///Copy Constructor
   TextureBasedUpdateSolutionEventHandler(const TextureBasedUpdateSolutionEventHandler& ceh);
   ///Destructor
   virtual ~TextureBasedUpdateSolutionEventHandler();

   ///Equal operator
   TextureBasedUpdateSolutionEventHandler& operator=(const TextureBasedUpdateSolutionEventHandler& rhs);
   
protected:
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void _operateOnNode(VE_XML::XMLObject* veXMLObject);
};
}
#endif// TEXTURE_BASED_ACTIVATE_EVENT_HANDLER_H
