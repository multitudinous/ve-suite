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
#ifndef DELETE_OBJECT_FROM_NETWORK_EVENT_HANDLER_H
#define DELETE_OBJECT_FROM_NETWORK_EVENT_HANDLER_H
/*!\file DeleteObjectFromNetworkEventHandler.h
  DeleteObjectFromNetworkEventHandler API
  */
/*!\class DeleteObjectFromNetworkEventHandler
 * Class for deleting objects from network.
 */
//#include <string>
//#include <vector>
#include <map>
#include <ves/VEConfig.h>
//#include <utility>

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

namespace ves
{
namespace xplorer
{
class GlobalBase;
}
}

namespace ves
{
namespace xplorer
{
namespace plugin
{
class PluginBase;
}
}
}
#include <ves/xplorer/event/EventHandler.h>
namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS DeleteObjectFromNetworkEventHandler: public ves::xplorer::event::EventHandler
{
public:
    ///Constructor
    DeleteObjectFromNetworkEventHandler();

    ///Copy Constructor
    DeleteObjectFromNetworkEventHandler( const DeleteObjectFromNetworkEventHandler& rhs );

    ///Destructor
    virtual ~DeleteObjectFromNetworkEventHandler();

    ///Equal operator
    DeleteObjectFromNetworkEventHandler& operator=( const DeleteObjectFromNetworkEventHandler& rhs );

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

private:
};
}
}
}
#endif// DELETE_OBJECT_FROM_NETWORK_EVENT_HANDLER_H
