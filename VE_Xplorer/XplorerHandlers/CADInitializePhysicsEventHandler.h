/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2007-03-18 11:01:44 -0500 (Sun, 18 Mar 2007) $
 * Version:       $Rev: 7159 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: CADInitializePhysicsEventHandler.h 7159 2007-03-18 16:01:44Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_INITIALIZE_PHYSICS_EVENT_HANDLER_H
#define CAD_INITIALIZE_PHYSICS_EVENT_HANDLER_H

/*!\file CADToggleEventHandler.h
*/

/*!\class CADInitializePhysicsEventHandler
*Class to initialize phyics for a CADNode
*/

namespace VE_XML
{
    class XMLObject;
}

namespace VE_CAD
{
    class CADNode;
}

#include "VE_Xplorer/XplorerHandlers/CADEventHandler.h"

#include "VE_Installer/include/VEConfig.h"

namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS CADInitializePhysicsEventHandler : public CADEventHandler
{
public:
    ///Constructor
    CADInitializePhysicsEventHandler();

    ///Copy Constructor
    CADInitializePhysicsEventHandler( const CADInitializePhysicsEventHandler& rhs );

    ///Destructor
    virtual ~CADInitializePhysicsEventHandler();

    ///Equal operator
    CADInitializePhysicsEventHandler& operator=( const CADInitializePhysicsEventHandler& rhs );

protected:
    ///Toggle a CADNode on/off.
    ///\param command The Command containing the CADNode to toggle.
    void _operateOnNode(VE_XML::XMLObject* command);

};
}

#endif //CAD_INITIALIZE_PHYSICS_EVENT_HANDLER_H
