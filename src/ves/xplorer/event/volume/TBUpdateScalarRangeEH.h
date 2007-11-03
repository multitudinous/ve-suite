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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TEXTURE_BASED_UPDATE_SCALAR_RANGE_EVENT_HANDLER_H
#define TEXTURE_BASED_UPDATE_SCALAR_RANGE_EVENT_HANDLER_H
/*!\file TBUpdateScalarRangeEH.h
  TextureBasedUpdateScalarRangeEventHandler API
  */
/*!\class TextureBasedUpdateScalarRangeEventHandler
 * Activate the texture based visualization.
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

namespace ves
{
namespace xplorer
{
namespace event
{
class VE_XPLORER_EXPORTS TextureBasedUpdateScalarRangeEventHandler : public TextureBasedEventHandler
{
public:
   ///Constructor
   TextureBasedUpdateScalarRangeEventHandler();

   ///Copy Constructor
   TextureBasedUpdateScalarRangeEventHandler(const TextureBasedUpdateScalarRangeEventHandler& ceh);
   ///Destructor
   virtual ~TextureBasedUpdateScalarRangeEventHandler();

   ///Equal operator
   TextureBasedUpdateScalarRangeEventHandler& operator=(const TextureBasedUpdateScalarRangeEventHandler& rhs);

protected:
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void _operateOnNode(ves::open::xml::XMLObject* veXMLObject);
};

}
}
}

#endif// TEXTURE_BASED_UPDATE_SCALAR_RANGE_EVENT_HANDLER_H
