/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef VES_XPLORER_SCENEGRAPH_CAMERA_DEFINITIONS_H
#define VES_XPLORER_SCENEGRAPH_CAMERA_DEFINITIONS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

//Define camera constants
const double FOV_DEFAULT = 20.0;
const double ASPECT_RATIO_DEFAULT = 1.0;
const double NEAR_PLANE_DEFAULT = 0.1;
const double FAR_PLANE_DEFAULT = 2.0;

//Define camera enums w/ namespaces
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace camera
{

///
namespace Event
{
    enum Enum
    {
        NONE = 0x0,
        FOCUS = 0x1,
        //PUSH = 0x2,
        //DRAG = 0x4,
        RELEASE = 0x8
    };
}

} //end camera
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_CAMERA_DEFINITIONS_H
