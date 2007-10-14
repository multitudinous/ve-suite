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
#ifndef PHYSICS_SIMULATION_EVENT_HANDLER_H
#define PHYSICS_SIMULATION_EVENT_HANDLER_H

/*!\file DeviceEH.h
*/

/*!\class DeviceEventHandler
*Class for changing physics simulation events
*/

/*!\namespace VE_EVENTS
*
*/

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/EventHandler.h>

#include <ves/open/xml/XMLObjectPtr.h>

namespace VE_Xplorer
{
    class cfdGlobalBase;
}

namespace VE_EVENTS
{
class PhysicsSimulationEventHandler : public EventHandler
{
public:
    ///Constructor
    PhysicsSimulationEventHandler();

    ///Copy Constructor
    PhysicsSimulationEventHandler( const PhysicsSimulationEventHandler& ceh );

    ///Destructor
    virtual ~PhysicsSimulationEventHandler();

    ///Equal operator
    PhysicsSimulationEventHandler& operator=( const PhysicsSimulationEventHandler& rhs );

    ///Set the cfdModel
    ///\param modelHandler The cfdModelHandler to execute the Command on
    void SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( ves::open::xml::XMLObject* command ); 
   
protected:

};
}

#endif //PHYSICS_SIMULATION_EVENT_HANDLER_H
