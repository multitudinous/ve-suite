/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/

#ifndef VTK_ACTOR_TO_STREAMLINE_H
#define VTK_ACTOR_TO_STREAMLINE_H

#include <ves/VEConfig.h>

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ref_ptr>

#include <vtkActor.h>
#include <vtkCellArray.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
osg::Geode* vtkActorToStreamLine( vtkActor* actor, osg::Geode* geode = NULL, int verbose = 0 );

osg::Geometry* ProcessPrimitive( vtkActor *a, vtkCellArray *prims, int pType, int v );

osg::Program* GetShader();
}
}
}

#endif //VTK_ACTOR_TO_STREAMLINE_H
