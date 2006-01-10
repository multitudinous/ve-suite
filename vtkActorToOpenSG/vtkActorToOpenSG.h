/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VTK_TO_OSG_H
#define VTK_TO_OSG_H

#include <OpenSG/OSGGeometry.h>
class vtkActor;

// Activate the OpenSG namespace
OSG_USING_NAMESPACE

// Takes a VTK actor and converts it to an OpenSG Geometry pointer
// The option parameter specifies what type of geometry to be used:
// 0 for GL_TRIANGLES, 1 for GL_POLYGON, 2 for GL_QUADS, or anything
// else for GL_POINTS. Note that GL_TRIANGLES is the most popular option
// followed by GL_POLYGON, etc.
GeometryPtr convert( vtkActor *actor, int choice );

#endif
