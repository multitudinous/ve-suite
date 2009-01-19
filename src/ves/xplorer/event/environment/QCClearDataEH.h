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
#ifndef QUAT_CAM_CLEAR_DATA_H
#define QUAT_CAM_CLear_DATA_H

#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file QCClearDataEH.h
  QuatCamClearDataEventHandler API
  */
/*!\class QuatCamClearDataEventHandler
 * Activate the texture based visualization.
 */
class VE_XPLORER_EXPORTS QuatCamClearDataEventHandler : public EventHandler
{
public:
    ///Constructor
    QuatCamClearDataEventHandler();

    ///Copy Constructor
    QuatCamClearDataEventHandler( const QuatCamClearDataEventHandler& ceh );
    ///Destructor
    virtual ~QuatCamClearDataEventHandler();

    ///Equal operator
    QuatCamClearDataEventHandler& operator=( const QuatCamClearDataEventHandler& rhs );
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

#endif// QUAT_CAM_LOAD_FILE_H
