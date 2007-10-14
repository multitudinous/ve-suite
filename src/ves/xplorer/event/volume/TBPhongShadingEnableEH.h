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
#ifndef TEXTURE_BASED_PHONG_SHADING_ENABLE_EVENT_HANDLER_H
#define TEXTURE_BASED_PHONG_SHADING_ENABLE_EVENT_HANDLER_H
/*!\file TBPhongShadingEnableEH.h
  TextureBasedPhongShadingEnableEventHandler API
  */
/*!\class TextureBasedPhongShadingEnableEventHandler
 * Activate phong shading on the texture based visualization.
 */

#include <ves/xplorer/event/volume/TextureBasedEventHandler.h>
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
#include <ves/VEConfig.h>

namespace VE_EVENTS{
class VE_XPLORER_EXPORTS TextureBasedPhongShadingEnableEventHandler : public TextureBasedEventHandler{
public:
   ///Constructor
   TextureBasedPhongShadingEnableEventHandler();

   ///Copy Constructor
   TextureBasedPhongShadingEnableEventHandler(const TextureBasedPhongShadingEnableEventHandler& ceh);
   ///Destructor
   virtual ~TextureBasedPhongShadingEnableEventHandler();

   ///Equal operator
   TextureBasedPhongShadingEnableEventHandler& operator=(const TextureBasedPhongShadingEnableEventHandler& rhs);
   
protected:
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void _operateOnNode(ves::open::xml::XMLObject* veXMLObject);
};
}
#endif// TEXTURE_BASED_PHONG_SHADING_ENABLE_EVENT_HANDLER_H
