/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdTransientParticleActor.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdTransientParticleActor.h"
#include "cfdDataSet.h"

#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkDataSet.h>
#include <vtkGeometryFilter.h>
#include <vpr/Util/Debug.h>
#include <vtkGlyph3D.h>
#include <vtkSphereSource.h>

void cfdTransientParticleActor::definePipeline( )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdTransientParticleActor::definePipeline"
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,2) << "this->dataset = " << this->dataset
                          << std::endl << vprDEBUG_FLUSH;

   vtkGeometryFilter * geoFilter = vtkGeometryFilter::New();
   geoFilter->SetInput( this->dataset );
   geoFilter->Update();

   //if ( this->option == PARTICLE_SPHERES )
   if ( 0 )
   {
      vprDEBUG(vprDBG_ALL,0) << " Creating a sphere data source"
                             << std::endl << vprDEBUG_FLUSH;
      vtkGlyph3D * sphereGlyph = vtkGlyph3D::New();
      vtkSphereSource * sphereSrc   = vtkSphereSource::New();
      //this->sphereSrc->SetRadius( 0.5f );

      sphereGlyph->SetInput( geoFilter->GetOutput() );
      sphereGlyph->SetSource( sphereSrc->GetOutput() );
      sphereGlyph->Update();

      //sphereGlyph->SelectInputScalars("Particle_Diameter");
      vprDEBUG(vprDBG_ALL,0) << " Using scalar data from "
         << this->dataset->GetPointData()->GetScalars()->GetName()
         << std::endl << vprDEBUG_FLUSH;
      sphereGlyph->SelectInputScalars( this->dataset->GetPointData()->GetScalars()->GetName() );
      sphereGlyph->SetScaleModeToScaleByScalar();
      sphereGlyph->SetScaleFactor( 300.0 );

      this->mapper->SetInput( sphereGlyph->GetOutput() );
      sphereSrc->Delete();
      sphereGlyph->Delete();
   }
   else
      this->mapper->SetInput( geoFilter->GetOutput() );

   geoFilter->Delete();   
   
   // Specify the lookup table
   vtkLookupTable * lut = vtkLookupTable::New();
   lut->SetNumberOfColors(256); //default is 256
   if ( this->dataset->GetPointData()->GetScalars()->GetLookupTable() )
   {
      vprDEBUG(vprDBG_ALL,0) 
         << " A lookup table is being read from the vtk file"
         << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      this->mapper->SetColorModeToMapScalars();
/*
      this->mapper->SetScalarRange( this->minmax );
      vprDEBUG(vprDBG_ALL, 0) 
         << " A blue-to-red lookup table is constructed from scalar min max : " 
         << this->minmax[0] << " : " << this->minmax[1]
         << std::endl << vprDEBUG_FLUSH;
*/
      this->mapper->SetScalarRange( this->activeDataSet->GetUserRange() );
      lut->SetHueRange(2.0f/3.0f, 0.0f); //a blue-to-red scale
      //lut->SetTableRange( this->minmax );
      lut->SetTableRange( this->activeDataSet->GetUserRange() );
      lut->Build();
   }

   this->mapper->SetLookupTable( lut );
   this->mapper->Update();
   lut->Delete();
}

