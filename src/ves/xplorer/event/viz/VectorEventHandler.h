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
#ifndef VECTOR_EVENT_HANDLER_H
#define VECTOR_EVENT_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/EventHandler.h>

#include <ves/open/xml/DataValuePairPtr.h>

#include <vector>
#include <string>

namespace ves
{
namespace xplorer
{
class cfdGraphicsObject;
namespace event
{
/*!\file VectorEventHandler.h
 * VectorEventHandler API
 */

/*!\class VectorEventHandler
 * Class for changing streamline properties in xplorer
 */
class VectorEventHandler : public EventHandler
{
public:
    //Constructor
    VectorEventHandler();

    //Copy Constructor
    VectorEventHandler( const VectorEventHandler& ceh );

    //Destructor
    virtual ~VectorEventHandler();

    //Set the cfdModel
    //param model The ModelHandler to execute the Command on
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler );

    //Exectute the event
    //param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

    //Equal operator
    VectorEventHandler& operator=( const VectorEventHandler& rhs );

protected:

    void UpdateGeodeUniform( 
        const std::vector< ves::xplorer::cfdGraphicsObject* >& graphicsObject, 
        ves::open::xml::DataValuePairPtr dvp, 
        const std::string& uniformName, double valueFactor );
};
}
}
}

#endif // end VECTOR_EVENT_HANDLER_H
