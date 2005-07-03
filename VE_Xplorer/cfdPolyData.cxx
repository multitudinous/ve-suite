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
 * File:          $RCSfile: cfdPolyData.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdPolyData.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdEnum.h"

#include <vtkTubeFilter.h>
#include <vtkCellTypes.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkWarpVector.h>

// following is for vertex-based sphere glyphs
#include <vtkGlyph3D.h>
#include <vtkSphereSource.h>
#include <vtkPointData.h>

#include <vpr/Util/Debug.h>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdPolyData::cfdPolyData( float op_val )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdPolyData constructor"
                          << std::endl << vprDEBUG_FLUSH;

   this->map = vtkPolyDataMapper::New();
   this->map->SetColorModeToMapScalars();
   warper = vtkWarpVector::New();
   warpSurface = false;
   warpedContourScale = 1.0f;

   _particleOption = 0;
   _particleScale = 1;
   //this->map->ScalarVisibilityOff();
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
   vprDEBUG(vprDBG_ALL,2) << "cfdPolyData destructor"
                          << std::endl << vprDEBUG_FLUSH;

   this->map->Delete();
   warper->Delete();
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
      std::cerr << "ERROR: Activate a polydata file to use this function" 
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

   vtkActor* temp = vtkActor::New();
   vtkPolyData * pd = this->GetActiveDataSet()->GetPolyData();   
   vtkCellTypes *types = vtkCellTypes::New();
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
	   temp->GetProperty()->SetRepresentationToSurface();
   }
   else if ( pd->GetCellType( 0 ) == VTK_VERTEX &&
             types->GetNumberOfTypes() == 1 &&
             GetParticleOption() == 1 )
   {
      vprDEBUG(vprDBG_ALL,1) << " IS VERTEX-BASED: variably sized spheres"
                             << std::endl << vprDEBUG_FLUSH;

      vtkSphereSource * sphereSrc   = vtkSphereSource::New();
      sphereSrc->SetThetaResolution( 3 ); // default is 8
      sphereSrc->SetPhiResolution( 3 );   // default is 8
      //this->sphereSrc->SetRadius( 0.5f );

      vtkGlyph3D * sphereGlyph = vtkGlyph3D::New();
      sphereGlyph->SetInput( pd );
      sphereGlyph->SetSource( sphereSrc->GetOutput() );
      sphereGlyph->Update();

      vprDEBUG(vprDBG_ALL,1) << " Using scalar data from "
         << pd->GetPointData()->GetScalars()->GetName()
         << std::endl << vprDEBUG_FLUSH;
      sphereGlyph->SelectInputScalars( pd->GetPointData()->GetScalars()->GetName() );
      sphereGlyph->SetScaleModeToScaleByScalar();
      sphereGlyph->SetColorModeToColorByScalar();

      // this attempts to set size of largest particle proportional
      // to the diagonal length of the entire dataset

      float len = this->GetActiveDataSet()->GetLength();
      // if there is only one point, then len equals zero...
      if ( len == 0.0 ) 
         len = 1.0;
      
      vprDEBUG(vprDBG_ALL,2) << " diagonalLength = " << len
                             << std::endl << vprDEBUG_FLUSH;

      int numPts = this->GetActiveDataSet()->GetDataSet()->GetNumberOfPoints();
      vprDEBUG(vprDBG_ALL,2) << " numPts = " << numPts
                             << std::endl << vprDEBUG_FLUSH;
      float scaleFactor = 0.0;
      if ( numPts != 0 )
      {
         scaleFactor = this->GetSphereScaleFactor()*20.0*len / (float)numPts;
      }
      sphereGlyph->SetScaleFactor( scaleFactor );

      sphereGlyph->ClampingOn();
      double range[ 2 ];
      this->GetActiveDataSet()->GetParent()->GetUserRange( range );
      // move bottom of range back 10% so that low valued spheres do not completely disappear
      range[0] = range[0] - ( range[1] - range[0] ) * 0.1;
      vprDEBUG(vprDBG_ALL,1) << " clamping range: "
         << range[0] << " : " << range[1]
         << std::endl << vprDEBUG_FLUSH;
      sphereGlyph->SetRange( range );
      //sphereGlyph->SetRange( this->GetActiveDataSet()->GetParent()->GetUserRange() );

      this->map->SetInput( sphereGlyph->GetOutput() );
      sphereSrc->Delete();
      sphereGlyph->Delete();
      temp->GetProperty()->SetRepresentationToSurface();
   }
   else if ( pd->GetCellType( 0 ) == VTK_VERTEX &&
             types->GetNumberOfTypes() == 1 &&
             GetParticleOption() == 0 )
   {
      vprDEBUG(vprDBG_ALL,1) << " IS VERTEX-BASED: point cloud"
                             << std::endl << vprDEBUG_FLUSH;
      this->map->SetColorModeToMapScalars();
      this->map->SetInput( pd );
      temp->GetProperty()->SetRepresentationToPoints();
      temp->GetProperty()->SetPointSize(4*this->GetSphereScaleFactor());
   }
   else
   {
      vprDEBUG(vprDBG_ALL,1) << " IS POLYDATA SURFACE"
                             << std::endl << vprDEBUG_FLUSH;
      if ( warpSurface )
      {
         this->warper->SetInput( pd );
         this->warper->SetScaleFactor( this->warpedContourScale );
         this->warper->Update();//can this go???
         this->map->SetInput( (vtkPolyData*)warper->GetOutput() );
         warpSurface = false;
      }
      else
      {
         this->map->SetInput( pd );
      }
      temp->GetProperty()->SetRepresentationToSurface();
   }

   types->Delete();

   if ( pd->GetPointData()->GetScalars()->GetLookupTable() != NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << " A lookup table (" 
         << pd->GetPointData()->GetScalars()->GetLookupTable()
         << ")is being read from the vtk file" 
         << std::endl << vprDEBUG_FLUSH;
      double range[ 2 ];
      pd->GetPointData()->GetScalars()->GetRange( range );
      this->map->SetScalarRange( range );
      this->map->SetLookupTable( pd->GetPointData()->GetScalars()->GetLookupTable() );
   }
   else
   {
      double * range = this->GetActiveDataSet()->GetParent()->GetUserRange();
      vprDEBUG(vprDBG_ALL,1) << "setting mapper using parent " 
         << this->GetActiveDataSet()->GetParent()
         << ", range = " << range[0] << " : " << range[1]
         << std::endl << vprDEBUG_FLUSH;
      
      this->map->SetScalarRange( this->GetActiveDataSet()
                                     ->GetParent()->GetUserRange() );
      this->map->SetLookupTable( this->GetActiveDataSet()
                                     ->GetParent()->GetLookupTable() );
   }

   this->map->Update();

   temp->SetMapper( this->map );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   geodes.push_back( new VE_SceneGraph::cfdGeode() );
   geodes.back()->TranslateTocfdGeode( temp );
   temp->Delete();
   this->updateFlag = true;
}

bool cfdPolyData::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   // Fix this
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) == 1 &&
         commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == POLYDATA)
   {
      warpSurface = true;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
               == CHANGE_CONTOUR_SETTINGS )
   {  
      // warped contour settings
      //double v[2];
	  // this->GetActiveDataSet()->GetParent()->GetUserRange( v );
      int scale = commandArray->GetCommandValue( cfdCommandArray::CFD_MIN );
      this->warpedContourScale = (scale/50.0) * 0.1 
                    * this->GetActiveDataSet()->GetParent()->GetLength();///(float)(v[1]-v[0]);

      // contour lod control
      /*int lod = commandArray->GetCommandValue( cfdCommandArray::CFD_MAX );
      double realLOD = (double)lod/100.0;
      this->deci->SetTargetReduction( realLOD );*/
      return true;
   }
   else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_PARTICLE_VIEW_OPTION )
   {
      SetParticleOption(commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE ) );

      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PARTICLE_VIEW_OPTION, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
         << std::endl << vprDEBUG_FLUSH;

      vprDEBUG(vprDBG_ALL,0) << " CHANGE_SPHERE_SIZE, value = " 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      SetParticleScale( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   return flag;
}
//////////////////////////////////////////////////////////
void cfdPolyData::SetParticleOption( unsigned int option )
{
   _particleOption = option;
}
//////////////////////////////////////////////
unsigned int cfdPolyData::GetParticleOption()
{
   return _particleOption;
}
//////////////////////////////////////////////
void cfdPolyData::SetParticleScale( float x )
{
   _particleScale = x;
}
/////////////////////////////////////
float cfdPolyData::GetParticleScale()
{
   return _particleScale;
}
/////////////////////////////////
void cfdPolyData::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}

float cfdPolyData::GetSphereScaleFactor()
{
   // this->GetParticleScale() is obtained from gui, -100 < sphereScale < 100
   // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
   // convert range to -4 < x < 4, and compute the exponent...
   vprDEBUG(vprDBG_ALL,1) << " sphereScale = " << this->GetParticleScale()
                          << std::endl << vprDEBUG_FLUSH;

   float scaleFactor = 0.0;

   if ( -100 <= this->GetParticleScale() && this->GetParticleScale() <= 100 )
   {
      scaleFactor = exp( this->GetParticleScale() / 25.0 );
   }

   vprDEBUG(vprDBG_ALL,1) << " scaleFactor = " << scaleFactor 
                          << std::endl << vprDEBUG_FLUSH;

   return scaleFactor;
}

