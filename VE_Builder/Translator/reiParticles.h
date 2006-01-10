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
 * File:          $RCSfile: reiParticles.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef REI_PARTICLES_H
#define REI_PARTICLES_H

class vtkPolyDataWriter;
class vtkPoints;
class vtkCellArray;
class vtkTransform;
class vtkTransformPolyDataFilter;
class vtkFloatArray;
class vtkPolyData;
class vtkTubeFilter;
class vtkPolyDataMapper;
class vtkActor;
class Particle;
#include <vector>


//! VTK streamers plane renderer.
/*!
  A class to takes input data set(s) and generates streamlines 
  based on the glyph given. Update member function will be update
  the position and direction as each "Update" being called.
*/
class reiParticles
{
   public:
      reiParticles( void );
      ~reiParticles( void );
      reiParticles( reiParticles * );

      void writeParticlePolyData( void );
      void readPPLOT1( void );
      void readPPLOT3( void );
      void readParticleParamFile( void );

      typedef std::vector< Particle * > Particles;
      Particles particles;
      int nsl;
      int nps;

      vtkPolyDataWriter          *writer;      
      vtkPolyData                *polydata;          
      vtkPoints                  *points;              
      vtkCellArray               *lines;            
      vtkFloatArray              **parameterData;          
      vtkTransform               *transform;        
      vtkTransformPolyDataFilter *transFilter; 
      //typedef std::vector< int > IntList;
      //IntList whichParticles;
};
#endif
