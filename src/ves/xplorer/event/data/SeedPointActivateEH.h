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
#ifndef SEED_POINT_ACTIVATE_EVENT_HANDLER_H
#define SEED_POINT_ACTIVATE_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/ModelPtr.h>
#include <ves/open/xml/XMLObjectPtr.h>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file SeedPointActivateEH.h
  SeedPointActivateEventHandler API
  */
/*!\class SeedPoint ActivateEventHandler
 * Turn on/off display of seed points.
 */
class VE_XPLORER_EXPORTS SeedPointActivateEventHandler: public EventHandler
{
public:
    ///Constructor
    SeedPointActivateEventHandler();

    ///Copy Constructor
    SeedPointActivateEventHandler( const SeedPointActivateEventHandler& ceh );
    ///Destructor
    virtual ~SeedPointActivateEventHandler();

    ///Equal operator
    SeedPointActivateEventHandler& operator=( const SeedPointActivateEventHandler& rhs );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    virtual void Execute( const ves::open::xml::XMLObjectPtr& xmlObject );

    ///Set the active model
    ///\param baseObject Active model
    virtual void SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject );

protected:
    ves::xplorer::Model* _activeModel;///<The active cfdModel
};

}
}
}

#endif//SEED_POINT_ACTIVATE_EVENT_HANDLER_H
