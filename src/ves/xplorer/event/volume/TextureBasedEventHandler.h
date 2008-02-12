/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef TEXTURE_BASED_EVENT_HANDLER_H
#define TEXTURE_BASED_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/ModelPtr.h>
#include <ves/open/xml/XMLObjectPtr.h>

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureDataSet;
}
}
}


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file TextureBasedEventHandler.h
  TextureBasedEventHandler API
  */
/*!\class TextureBasedEventHandler
 * Base class for TextureBased Visualization event handling.
 */
class VE_XPLORER_EXPORTS TextureBasedEventHandler : public EventHandler
{
public:
    ///Constructor
    TextureBasedEventHandler();

    ///Copy Constructor
    TextureBasedEventHandler( const TextureBasedEventHandler& ceh );

    ///Destructor
    virtual ~TextureBasedEventHandler();

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( ves::open::xml::XMLObjectPtr command );

    ///Equal operator
    TextureBasedEventHandler& operator=( const TextureBasedEventHandler& rhs );

protected:
    ///The internal operation on the CADNode.
    ///\param veXMLObject The veXMLObject to execute.
    virtual void _operateOnNode( ves::open::xml::XMLObjectPtr veXMLObject ) = 0;

    ///Set the active cfdTextureDataset
    void _setActiveTextureDataset( /*std::string tdsName*/ );
    ves::xplorer::volume::cfdTextureDataSet* _activeTDSet;///<The active cfdTextureDataset.
    ves::xplorer::Model* _activeModel;///<The active cfdModel;
};

}
}
}

#endif// TEXTURE_BASED_EVENT_HANDLER_H
