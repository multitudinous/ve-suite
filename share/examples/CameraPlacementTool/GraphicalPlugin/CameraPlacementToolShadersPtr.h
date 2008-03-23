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
 * Date modified: $Date: 2008-03-13 21:07:47 -0500 (Thu, 13 Mar 2008) $
 * Version:       $Rev: 10811 $
 * Author:        $Author: dshipton $
 * Id:            $Id: Command.h 10811 2008-03-14 02:07:47Z dshipton $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H
#define CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H

#include <ves/util/PointerTypes.h>

namespace cpt
{

class CameraPlacementToolShaders;

//Typedef for a SmartPtr type for the CameraPlacementToolScene
typedef ves::util::ClassPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersPtr;
typedef ves::util::SharedPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersSharedPtr;
typedef ves::util::WeakPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersWeakPtr;
typedef ves::util::ScopedPtrDef< CameraPlacementToolShaders >::type
    CameraPlacementToolShadersScopedPtr;

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SHADERS_PTR_H
