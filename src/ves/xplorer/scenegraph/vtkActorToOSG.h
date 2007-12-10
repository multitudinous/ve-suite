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
//C++ header - fIVE|Analyse - Copyright (C) 2002-2003 Michael Gronager, UNI-C
//Distributed under the terms of the GNU Library General Public License (LGPL)
//as published by the Free Software Foundation.

#ifndef VTKACTORTOOSG_H
#define VTKACTORTOOSG_H
#ifdef _OSG
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ref_ptr>

#include <vtkActor.h>
#include <vtkCellArray.h>
#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
// vtkActorToOSG - translates vtkActor to osg::Geode. If geode is NULL, new one
//   will be created. Optional verbose parameter prints debugging and
//   performance information.
osg::ref_ptr< osg::Geode > vtkActorToOSG( vtkActor* actor, osg::ref_ptr< osg::Geode > geode = NULL, int verbose = 0 );

osg::ref_ptr< osg::Geometry > processPrimitive( vtkActor *a, vtkCellArray *prims, int pType, int v );
}
}
}

#endif
#endif
