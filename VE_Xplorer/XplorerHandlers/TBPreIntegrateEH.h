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
 * Date modified: $Date: 2007-03-18 11:01:44 -0500 (Sun, 18 Mar 2007) $
 * Version:       $Rev: 7159 $
 * Author:        $Author: mccdo $
 * Id:            $Id: TBPreIntegrateEnableEH.h 7159 2007-03-18 16:01:44Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXTURE_BASED_PREINTEGRATE_ENABLE_EVENT_HANDLER_H
#define TEXTURE_BASED_PREINTEGRATE_ENABLE_EVENT_HANDLER_H
/*!\file TBPreIntegrateEH.h
  TextureBasedPreIntegrateEnableEventHandler API
  */
/*!\class TextureBasedPreIntegrateEnableEventHandler
 * Activate the texture based visualization.
 */

#include "VE_Xplorer/XplorerHandlers/TextureBasedEventHandler.h"
namespace VE_XML
{
   class XMLObject;
}
#include "VE_Installer/include/VEConfig.h"

namespace VE_EVENTS{
class VE_XPLORER_EXPORTS TextureBasedPreIntegrateEnableEventHandler : public TextureBasedEventHandler{
public:
   ///Constructor
   TextureBasedPreIntegrateEnableEventHandler();

   ///Copy Constructor
   TextureBasedPreIntegrateEnableEventHandler(const TextureBasedPreIntegrateEnableEventHandler& ceh);
   ///Destructor
   virtual ~TextureBasedPreIntegrateEnableEventHandler();

   ///Equal operator
   TextureBasedPreIntegrateEnableEventHandler& operator=(const TextureBasedPreIntegrateEnableEventHandler& rhs);
   
protected:
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void _operateOnNode(VE_XML::XMLObject* veXMLObject);
};
}
#endif// TEXTURE_BASED_PREINTEGRATE_ENABLE_EVENT_HANDLER_H
