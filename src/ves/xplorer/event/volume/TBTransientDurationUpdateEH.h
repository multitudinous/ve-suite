/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef TEXTURE_BASED_TRANSIENT_DURATION_UPDATE_EVENT_HANDLER_H
#define TEXTURE_BASED_TRANSIENT_DURATION_UPDATE_EVENT_HANDLER_H

#include <ves/xplorer/event/volume/TextureBasedEventHandler.h>
#include <ves/open/xml/XMLObjectPtr.h>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file TBTransientDurationUpdateEH.h
  TextureBasedTransientDurationUpdateEventHandler API
  */
/*!\class TextureBasedTransientDurationUpdateEventHandler
 * Activate the texture based visualization.
 */
class VE_XPLORER_EXPORTS TextureBasedTransientDurationUpdateEventHandler : public TextureBasedEventHandler
{
public:
    ///Constructor
    TextureBasedTransientDurationUpdateEventHandler();

    ///Copy Constructor
    TextureBasedTransientDurationUpdateEventHandler( const TextureBasedTransientDurationUpdateEventHandler& ceh );
    ///Destructor
    virtual ~TextureBasedTransientDurationUpdateEventHandler();

    ///Equal operator
    TextureBasedTransientDurationUpdateEventHandler& operator=( const TextureBasedTransientDurationUpdateEventHandler& rhs );

protected:
    ///The internal operation on the CADNode.
    ///\param veXMLObject The veXMLObject to execute.
    virtual void _operateOnNode( ves::open::xml::XMLObjectPtr veXMLObject );
};

}
}
}

#endif// TEXTURE_BASED_ISOSURFACE_UPDATE_EVENT_HANDLER_H
