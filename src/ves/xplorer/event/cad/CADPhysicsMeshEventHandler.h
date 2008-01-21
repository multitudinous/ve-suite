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
#ifndef CAD_PHYSICS_MESH_EVENT_HANDLER_H
#define CAD_PHYSICS_MESH_EVENT_HANDLER_H

#include <ves/xplorer/event/cad/CADEventHandler.h>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CADPhysicsMeshEventHandler.h
*/

/*!\class CADPhysicsMeshEventHandler
*Class to handle physics mesh types for CADEntities
*/
class VE_XPLORER_EXPORTS CADPhysicsMeshEventHandler : public CADEventHandler
{
public:
    ///Constructor
    CADPhysicsMeshEventHandler();

    ///Copy Constructor
    CADPhysicsMeshEventHandler( const CADPhysicsMeshEventHandler& rhs );

    ///Destructor
    virtual ~CADPhysicsMeshEventHandler();

    ///Equal operator
    CADPhysicsMeshEventHandler& operator=( const CADPhysicsMeshEventHandler& rhs );

protected:
    ///Toggle a CADNode on/off.
    ///\param command The Command containing the CADNode to toggle.
    void _operateOnNode( ves::open::xml::XMLObject* command );

};

}
}
}

#endif //CAD_PHYSICS_MESH_EVENT_HANDLER_H
