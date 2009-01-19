/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef DELETE_NETWORK_VIEW_EVENT_HANDLER_H
#define DELETE_NETWORK_VIEW_EVENT_HANDLER_H
/*!\file DeleteNetworkViewEventHandler.h
  DeleteNetworkViewEventHandler API
  */
/*!\class DeleteNetworkViewEventHandler
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

#include <ves/xplorer/event/EventHandler.h>
namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS DeleteNetworkViewEventHandler: public ves::xplorer::event::EventHandler
{
public:
    ///Constructor
    DeleteNetworkViewEventHandler();

    ///Copy Constructor
    DeleteNetworkViewEventHandler( const DeleteNetworkViewEventHandler& rhs );

    ///Destructor
    virtual ~DeleteNetworkViewEventHandler();

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Equal operator
    DeleteNetworkViewEventHandler& operator=( const DeleteNetworkViewEventHandler& rhs );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

private:
};
}
}
}
#endif// DELETE_NETWORK_VIEW_EVENT_HANDLER_H
