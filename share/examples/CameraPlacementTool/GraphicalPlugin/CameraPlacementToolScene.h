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

#ifndef CAMERA_PLACEMENT_TOOL_SCENE_H
#define CAMERA_PLACEMENT_TOOL_SCENE_H

// --- My Includes --- //
#include "CameraPlacementToolScenePtr.h"
#include "CameraPlacementToolShadersPtr.h"

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
}
}
}

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Includes --- //

namespace cpt
{
// --- My Includes --- //
class CameraEntity;

class CameraPlacementToolScene
{
public:
    CameraPlacementToolScene( ves::xplorer::scenegraph::DCS* pluginDCS );
    ~CameraPlacementToolScene();

    cpt::CameraEntity* GetActiveCameraEntity();

private:
    void Initialize();

    cpt::CameraPlacementToolShadersPtr mShaders;
    osg::ref_ptr< cpt::CameraEntity > mCameraEntity;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mTorus;

};

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_SCENE_H
