/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdPolyData.cxx,v $
 * Date modified: $Date: 2004/04/30 13:00:45 $
 * Version:       $Revision: 1.10 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPolyData.h"
#include "cfdDataSet.h"

#include <vtkTubeFilter.h>
#include <vtkCellTypes.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

// following is for vertex-based sphere glyphs
#include <vtkGlyph3D.h>
#include <vtkSphereSource.h>
#include <vtkPointData.h>

#include <vpr/Util/Debug.h>

cfdPolyData::cfdPolyData( float op_val )
{
   this->op = op_val;

   this->map = vtkPolyDataMapper::New();
   this->map->SetColorModeToMapScalars();
   //this->map->ScalarVisibilityOff();

   this->actor = vtkActor::New();

/*
   this->actor->GetProperty()->SetColor( 1.0f, 1.0f, 1.0f );
   this->actor->GetProperty()->SetAmbient( 0.2f );
   this->actor->GetProperty()->SetDiffuse( 0.8f );
   this->actor->GetProperty()->SetSpecular( 0.3f );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );
   this->actor->GetProperty()->SetOpacity( this->op );
*/
}

cfdPolyData::~cfdPolyData()
{
   this->map->Delete();
   this->actor->Delete();
}

void cfdPolyData::setOpacity( float op_val )
{
   this->op = op_val;
   this->actor->GetProperty()->SetOpacity( this->op );
}

float cfdPolyData::getOpacity()
{
   return this->op;
}

void cfdPolyData::Update()
{
   if ( this->GetActiveDataSet() == NULL )
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdPolyData has no data so setting updateFlag to false" 
         << std::endl << vprDEBUG_FLUSH;
      this->updateFlag = false;
      return;
   }
   else if ( ! this->GetActiveDataSet()->GetDataSet()->IsA("vtkPolyData") )
   {
      cout << "ERROR: Activate a polydata file to use this function" 
         << std::endl;
      this->updateFlag = false;
      return;
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 1) 
         << "cfdPolyData: this->GetActiveDataSet() = " 
         << this->GetActiveDataSet() << std::endl << vprDEBUG_FLUSH;
   }

   vtkCellTypes *types = vtkCellTypes::New();
   vtkPolyData * pd = this->GetActiveDataSet()->GetPolyData();   
   pd->GetCellTypes( types );   
   
   if ( pd->GetCellType( 0 ) == VTK_POLY_LINE &&
        types->GetNumberOfTypes() == 1 )
   {
      vprDEBUG(vprDBG_ALL,1) << " IS A STREAMLINE"
                             << std::endl << vprDEBUG_FLUSH;
      vtkTubeFilter * polyTubes = vtkTubeFilter::New();
      polyTubes->SetNumberOfSides( 3 );
      polyTubes->SetInput( pd );
      polyTubes->SetRadius( .05 );
      polyTubes->Update();
      this->map->SetInput( polyTubes->GetOutput() );
      polyTubes->Delete();
      this->actor->GetProperty()->SetRepresentationToSurface();
   }
   else if ( pd->GetCellType( 0 ) == VTK_VERTEX &&
             types->GetNumberOfTypes() == 1 &&
             GetSphereScale() != -999 )
   {
      vprDEBUG(vprDBG_ALL,1) << " IS VERTEX-BASED: spheres"
                             << std::endl << vprDEBUG_FLUSH;
      vtkGlyph3D * sphereGlyph = vtkGlyph3D::New();
      vtkSphereSource * sphereSrc   = vtkSphereSource::New();
      sphereSrc->SetThetaResolution( 3 ); // default is 8
      sphereSrc->SetPhiResolution( 3 );   // default is 8
      //this->sphereSrc->SetRadius( 0.5f );

      sphereGlyph->SetInput( pd );
      sphereGlyph->SetSource( sphereSrc->GetOutput() );
      sphereGlyph->Update();

      vprDEBUG(vprDBG_ALL,1) << " Using scalar data from "
         << pd->GetPointData()->GetScalars()->GetName()
         << std::endl << vprDEBUG_FLUSH;
      sphereGlyph->SelectInputScalars( pd->GetPointData()->GetScalars()->GetName() );
      sphereGlyph->SetScaleModeToScaleByScalar();
      sphereGlyph->SetColorModeToColorByScalar();
      sphereGlyph->SetScaleFactor( GetSphereScaleFactor() );
      sphereGlyph->ClampingOn();
      double range[ 2 ];
      this->GetActiveDataSet()->GetParent()->GetUserRange( range );
      // move bottom of range back 10% so that low valued spheres do not completely disappear
      range[0] = range[0] - ( range[1] - range[0] ) * 0.1;
      vprDEBUG(vprDBG_ALL,1) << " clamping range: "
         << range[0] << " : " << range[1]
         << std::endl << vprDEBUG_FLUSH;
      sphereGlyph->SetRange( this->GetActiveDataSet()->GetParent()->GetUserRange() );

      this->map->SetInput( sphereGlyph->GetOutput() );
      sphereSrc->Delete();
      sphereGlyph->Delete();
      this->actor->GetProperty()->SetRepresentationToSurface();
   }
   else
   {
      vprDEBUG(vprDBG_ALL,1) << " NOT A STREAMLINE"
                             << std::endl << vprDEBUG_FLUSH;
      this->map->SetInput( pd );
      this->actor->GetProperty()->SetRepresentationToPoints();
      this->actor->GetProperty()->SetPointSize( 4 );
   }
   types->Delete();


   if ( pd->GetPointData()->GetScalars()->GetLookupTable() != NULL )
   {
      vprDEBUG(vprDBG_ALL,0)
         << " A lookup table is being read from the vtk file" 
         << std::endl << vprDEBUG_FLUSH;
      double* range = new double [ 2 ];
      pd->GetPointData()->GetScalars()->GetRange( range );
      this->map->SetScalarRange( range );
      this->map->SetLookupTable( pd->GetPointData()->GetScalars()->GetLookupTable() );
      delete [] range;
   }
   else
   {
      double * junkRange = this->GetActiveDataSet()
                            ->GetParent()->GetUserRange();
      vprDEBUG(vprDBG_ALL,1) << "setting mapper using parent " 
         << this->GetActiveDataSet()->GetParent()
         << ", range = " << junkRange[0] << " : " << junkRange[1]
         << std::endl << vprDEBUG_FLUSH;
      
      this->map->SetScalarRange( this->GetActiveDataSet()
                                  ->GetParent()->GetUserRange() );
      this->map->SetLookupTable( this->GetActiveDataSet()
                                  ->GetParent()->GetLookupTable() );
   }

   this->map->Update();

   this->actor->SetMapper( this->map );

   this->updateFlag = true;
}

float cfdPolyData::GetSphereScaleFactor()
{
   // this->GetSphereScale() is obtained from gui, -100 < sphereScale < 100
   // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
   // convert range to -2 < x < 2, and compute the exponent...
   vprDEBUG(vprDBG_ALL,1) << " sphereScale() = " << this->GetSphereScale()
                          << std::endl << vprDEBUG_FLUSH;

   float scaleFactor;

   if ( this->GetSphereScale() < -100 || this->GetSphereScale() > 100 )
   {
      scaleFactor = 0.0;
   }
   else
   {
      float len = this->GetActiveDataSet()->GetLength();
      // if there is only one point, then len equals zero...
      if ( len == 0.0 ) 
         len = 1.0;
      
      vprDEBUG(vprDBG_ALL,1) << " bbDiagonalLength = " << len
                             << std::endl << vprDEBUG_FLUSH;
      int numPts = this->GetActiveDataSet()->GetDataSet()->GetNumberOfPoints();
      vprDEBUG(vprDBG_ALL,1) << " numPts = " << numPts
                             << std::endl << vprDEBUG_FLUSH;
      if ( numPts == 0 )
         scaleFactor = 0.0;
      else
      {
         scaleFactor = exp( this->GetSphereScale() / 50.0 ) * 
                            20.0 * len / (float)numPts;
      }
   }

   vprDEBUG(vprDBG_ALL,1) << " scaleFactor = " << scaleFactor 
                          << std::endl << vprDEBUG_FLUSH;

   return scaleFactor;
}

