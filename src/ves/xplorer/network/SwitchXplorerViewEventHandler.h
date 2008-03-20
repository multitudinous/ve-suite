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
#ifndef SWITCH_XPLORER_VIEW_EVENT_HANDLER_H
#define SWITCH_XPLORER_VIEW_EVENT_HANDLER_H

/*!\file SwitchXplorerViewEventHandler.h
 */

/*!\class SwitchXplorerViewEventHandler
 * Class for switching the view in xplorer.
 */

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/network/NetworkSystemView.h>
#include <ves/open/xml/CommandPtr.h>

// --- C/C++ Libraries --- //
#include <map>
//#include <string>
//#include <vector>
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
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS SwitchXplorerViewEventHandler: public ves::xplorer::event::EventHandler
{
public:
    ///Constructor
    SwitchXplorerViewEventHandler();

    ///Constructor to pass the network
    //SwitchXplorerViewEventHandler(ves::open::xmlVE_Model::Network*);
    SwitchXplorerViewEventHandler( std::string );

    ///Copy Constructor
    SwitchXplorerViewEventHandler( const SwitchXplorerViewEventHandler& rhs );

    ///Destructor
    virtual ~SwitchXplorerViewEventHandler();

    ///Equal operator
    SwitchXplorerViewEventHandler& operator=( const SwitchXplorerViewEventHandler& rhs );

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

private:
    NetworkSystemView * networkLayout;
    void UpdateNetworkView( const ves::open::xml::CommandPtr& cmd );

};
}
}
}
#endif// SWITCH_XPLORER_VIEW_EVENT_HANDLER_H
