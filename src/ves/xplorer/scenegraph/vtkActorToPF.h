/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> **************/
//=========================================================================
//#
//#  vtkActorToPF.h                                   ##   ##
//#                                                   ##   ##
//#  Authors: Paul Rajlich                            #######
//#  Email: prajlich@ncsa.uiuc.edu                    ##   ##
//#  Date created:                                    ##   ##
//#
//#  Description: function prototypes
//#
//=========================================================================
//# Copyright (C) 1998-2000 Board of Trustees of the University of Illinois
//=========================================================================

#ifndef VTK_ACTOR_TO_PF_H
#define VTK_ACTOR_TO_PF_H
#ifdef _PERFORMER
#include <Performer/pf/pfTraverser.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pfutil.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>

// VTK44 is a subset of VTK4, so turn VTK4 flag on if not already
#ifdef VTK44
#define VTK4
#endif //VTK44

#ifdef VTK4
#include <vtkCellArray.h>
#endif
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cmath>
#ifndef WIN32
#include <unistd.h>
#else
#include <windows.h>
#endif
#include <fcntl.h>
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
// vtkActorToPF - translates vtkActor to pfGeode. If geode is NULL, new one
//   will be created. Optional verbose parameter prints debugging and
//   performance information.
VE_SCENEGRAPH_EXPORTS pfGeode* vtkActorToPF( vtkActor *actor, pfGeode *geode = NULL, int verbose = 0 );

// helper functions
VE_SCENEGRAPH_EXPORTS void vtkActorToGeoSets( vtkActor *actor, pfGeoSet *gsets[], int verbose );
VE_SCENEGRAPH_EXPORTS pfGeoSet *processPrimitive( vtkActor *a, vtkCellArray *prims, int pType, int v );
VE_SCENEGRAPH_EXPORTS void updateTexture( vtkActor *actor, pfGeoSet *gset, pfGeoState *gstate, int v );
}
}
}

#endif //_PERFORMER
#endif //VTK_ACTOR_TO_PF_H
