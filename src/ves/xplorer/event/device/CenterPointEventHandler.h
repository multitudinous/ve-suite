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
#ifndef CENTER_POINT_EVENT_HANDLER_H
#define CENTER_POINT_EVENT_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CenterPointEventHandler.h
 *
 */
/*!\class CenterPointEventHandler
 *
 */
class CenterPointEventHandler : public EventHandler
{
public:
    //Constructor
    CenterPointEventHandler();

    //Copy Constructor
    CenterPointEventHandler( const CenterPointEventHandler& ceh );

    //Destructor
    virtual ~CenterPointEventHandler();

    //Set the cfdModel
    //param model The ModelHandler to execute the Command on
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler );

    //Exectute the event
    //param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

    //Equal operator
    CenterPointEventHandler& operator=( const CenterPointEventHandler& rhs );

protected:

private:

};
} //end event
} //end xplorer
} //end ves

#endif //CENTER_POINT_EVENT_HANDLER_H
