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
 * Date modified: $Date: 2008-02-18 14:37:44 -0600 (Mon, 18 Feb 2008) $
 * Version:       $Rev: 10639 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef SET_RESET_START_PSOSITION_EVENT_HANDLER_H
#define SET_RESET_START_PSOSITION_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file SetResetStartPositionEventHandler.h
  SetResetStartPositionEventHandler API
  */
/*!\class SetResetStartPositionEventHandler
 * load a stored scene.
 */
class VE_XPLORER_EXPORTS SetResetStartPositionEventHandler : public EventHandler
{
public:
    ///Constructor
    SetResetStartPositionEventHandler();

    ///Copy Constructor
    SetResetStartPositionEventHandler( const SetResetStartPositionEventHandler& ceh );
    ///Destructor
    virtual ~SetResetStartPositionEventHandler();

    ///Equal operator
    SetResetStartPositionEventHandler& operator=( const SetResetStartPositionEventHandler& rhs );
    ///\param baseObject The GlobalBase object to apply the command to.
    virtual void SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject = 0 );

    ///The internal operation on the CADNode.
    ///\param veXMLObject The veXMLObject to execute.
    virtual void Execute( const ves::open::xml::XMLObjectPtr& veXMLObject );
protected:

};

}
}
}

#endif// SET_RESET_START_PSOSITION_EVENT_HANDLER_H
