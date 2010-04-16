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

#ifndef VES_XPLORER_SCENEGRAPH_MASKS_H
#define VES_XPLORER_SCENEGRAPH_MASKS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

///Node masks for all selectable objects
namespace NodeMask
{
enum Enum
{
    NONE                              = 0x00000000,

    CAMERA_MANAGER                    = 0xefffffff,
    CAMERA_PAT                        = 0xdfffffff,
    CAMERA                            = 0xcfffffff,

    CHARACTER                         = 0xfeffffff,

    GRAPHICAL_PLUGIN_MANAGER          = 0xffefffff,

    MANIPULATOR_MANAGER               = 0xfffeffff,
    DRAGGER                           = 0xfffdffff,

    TERRAIN                           = 0xffffefff,

    ALL                               = 0xffffffff
};
}

///Traversal masks for all selectable objects
namespace TraversalMask
{
enum Enum
{
    NONE                              = ~NodeMask::NONE,

    CAMERA_MANAGER                    = ~NodeMask::CAMERA_MANAGER,
    CAMERA_PAT                        = ~NodeMask::CAMERA_PAT,
    CAMERA                            = ~NodeMask::CAMERA,

    CHARACTER                         = ~NodeMask::CHARACTER,

    GRAPHICAL_PLUGIN_MANAGER          = ~NodeMask::GRAPHICAL_PLUGIN_MANAGER,

    MANIPULATOR_MANAGER               = ~NodeMask::MANIPULATOR_MANAGER,
        DRAGGER                       = ~NodeMask::DRAGGER,

    TERRAIN                           = ~NodeMask::TERRAIN,

    ALL                               = ~NodeMask::ALL
};
}

} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MASKS_H
