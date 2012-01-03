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
#pragma once
/*!\file LoadVesFile.h
  LoadVesFile API
  */
/*!\class LoadVesFile
 * Do cleanup operations associated with loading new VES file.
 */

#include <ves/VEConfig.h>

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

#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS LoadVesFileEventHandler: public ves::xplorer::event::EventHandler
{
public:
    ///Constructor
    LoadVesFileEventHandler();

    ///Copy Constructor
    LoadVesFileEventHandler( const LoadVesFileEventHandler& rhs );

    ///Destructor
    virtual ~LoadVesFileEventHandler();

    ///Equal operator
    LoadVesFileEventHandler& operator=( const LoadVesFileEventHandler& rhs );

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

