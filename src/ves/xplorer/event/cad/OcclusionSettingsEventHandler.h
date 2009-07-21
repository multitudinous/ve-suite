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
#ifndef CAD_OCCLUSION_SETTINGS_EVENT_HANDLER_H
#define CAD_OCCLUSION_SETTINGS_EVENT_HANDLER_H

#include <ves/xplorer/event/cad/CADEventHandler.h>
#include <ves/open/xml/XMLObjectPtr.h>


namespace ves
{
namespace xplorer
{
namespace event
{
namespace cad
{
/*!\file OcclusionSettingsEventHandler.h
*/

/*!\class OcclusionSettingsEventHandler
*Class to handle physics properties for CADEntities
*/
class VE_XPLORER_EXPORTS OcclusionSettingsEventHandler : public CADEventHandler
{
public:
    ///Constructor
    OcclusionSettingsEventHandler();

    ///Copy Constructor
    OcclusionSettingsEventHandler( const OcclusionSettingsEventHandler& rhs );

    ///Destructor
    virtual ~OcclusionSettingsEventHandler();

    ///Equal operator
    OcclusionSettingsEventHandler& operator=( const OcclusionSettingsEventHandler& rhs );

protected:
    ///Toggle a CADNode on/off.
    ///\param command The Command containing the CADNode to toggle.
    void _operateOnNode( ves::open::xml::XMLObjectPtr command );

};
}
}
}    
}

#endif //CAD_OCCLUSION_SETTINGS_EVENT_HANDLER_H
