/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef fluent_PARTICLES_H
#define fluent_PARTICLES_H

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cerrno>
#include <cmath>
#include <map>
#include <fluentParticle.h>

#include <vtkPolyDataWriter.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkDataSetAttributes.h>
#include <vtkTransform.h>
#include <vtkObjectBase.h>
#include <vtkTransformPolyDataFilter.h>
//class vtkPolyDataWriter;
//class vtkPoints;
//class vtkCellArray;
//class vtkTransform;
//class vtkTransformPolyDataFilter;
//class vtkFloatArray;
//class vtkPolyData;
//class vtkTubeFilter;
//class vtkPolyDataMapper;
//class vtkActor;



//! VTK streamers plane renderer.
/*!
  A class to takes input data set(s) and generates streamlines
  based on the glyph given. Update member function will be update
  the position and direction as each "Update" being called.
*/
class fluentParticles
{
public:
    fluentParticles( void );
    ~fluentParticles( void );

    void translateFluentPartToVTK( void );
    void allocatePolydata( void );
    void addToPolydata( int );
    void writePolydata( int );
    void deallocatePolydata( void );


    std::multimap<int, fluentParticle> mfluentParticles;

    vtkPolyDataWriter          *writer;
    vtkPolyData                *polydata;
    vtkPoints                  *points;
    vtkFloatArray              **parameterData;
    vtkTransform               *transform;
    vtkTransformPolyDataFilter *transFilter;
    int particleID[500];

};
#endif
