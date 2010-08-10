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

#ifndef CAMERA_PLACEMENT_TOOL_EH_H
#define CAMERA_PLACEMENT_TOOL_EH_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/EventHandler.h>

// --- STL Includes --- //
#include <map>
#include <string>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace environment
{
/*!\file CameraPlacementEventHandler.h
 *
 */

/*!\class CameraPlacementEventHandler
 * Class for
 */

/*!\namespace ves::xplorer::event::environment
 *
 */
class CameraPlacementEventHandler : public ves::xplorer::event::EventHandler
{
public:
    ///
    CameraPlacementEventHandler();

    ///
    virtual ~CameraPlacementEventHandler();

    ///
    CameraPlacementEventHandler( const CameraPlacementEventHandler& ceh );

    ///
    CameraPlacementEventHandler& operator=( const CameraPlacementEventHandler& rhs );

    ///
    enum STRING_TO_INT_IDS
    {
        ADD_CAMERA_OBJECT,
        PREV_NEXT_CAMERA_OBJECT,
        DELETE_CAMERA_OBJECT,
        REMOVE_ALL_CAMERA_OBJECTS,

        DEPTH_OF_FIELD_EFFECT_ON_OFF,
        PROJECTION_EFFECT_ON_OFF,
        PROJECTION_EFFECT_OPACITY,

        CAMERA_WINDOW_ON_OFF,
        CAMERA_WINDOW_RESOLUTION,

        DEPTH_HELPER_WINDOW_ON_OFF,
        DEPTH_HELPER_WINDOW_RESOLUTION,

        CAMERA_GEOMETRY_ON_OFF,
        FRUSTUM_GEOMETRY_ON_OFF,

        PROJECTION_UPDATE,

        FOCAL_DISTANCE,
        FOCAL_RANGE,
        MAX_CIRCLE_OF_CONFUSION,
    };

    ///Set the cfdModel
    ///\param modelHandler The ModelHandler to execute the Command on
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler );
    
    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& veXMLObject );

protected:

private:
    ///
    std::map< const std::string, STRING_TO_INT_IDS > mCommandNameToInt;

};
} //end environment
} //end event
} //end xplorer
} //end ves

#endif //CAMERA_PLACEMENT_TOOL_EH_H
