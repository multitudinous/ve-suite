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
 * File:          $RCSfile: cfdContourBase.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdContourBase.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"

#include "VE_Xplorer/cfdDebug.h"

#include <vtkPolyData.h>
#include <vtkContourFilter.h>                // contour lines
#include <vtkBandedPolyDataContourFilter.h>  // banded contours
#include <vtkGeometryFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkLookupTable.h>
#include <vtkDecimatePro.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
{
   vprDEBUG(vesDBG,2) << "cfdContourBase constructor"
                          << std::endl << vprDEBUG_FLUSH;
   this->deci = vtkDecimatePro::New();
   
   this->filter = vtkGeometryFilter::New();
   this->cfilter = vtkContourFilter::New();              // for contourlines
   this->bfilter = vtkBandedPolyDataContourFilter::New();// for banded contours
   // turn clipping on to avoid unnecessary value generations with 
   // vtkBandedPolyDataContourFilter::GenerateValues().
   this->bfilter->ClippingOn();   
   this->tris = vtkTriangleFilter::New();
   this->strip = vtkStripper::New();

   this->mapper = vtkPolyDataMapper::New();
   this->mapper->SetInput( this->filter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();
   mapper->ImmediateModeRenderingOn();
   normals = vtkPolyDataNormals::New();

   this->warpedContourScale = 0.0f;
   this->contourOpacity = 0.0f;
   this->contourLOD = 1; 
}

cfdContourBase::~cfdContourBase()
{
   vprDEBUG(vesDBG,2) << "cfdContourBase destructor"
                          << std::endl  << vprDEBUG_FLUSH;

   this->filter->Delete();
   this->filter = NULL;
   
   this->cfilter->Delete();
   this->cfilter = NULL;
   
   this->bfilter->Delete();
   this->bfilter = NULL;

   this->tris->Delete();
   this->tris = NULL;

   this->strip->Delete();
   this->strip = NULL;
   
   this->mapper->Delete();
   this->mapper = NULL;

   this->deci->Delete();
   this->deci = NULL;

   normals->Delete();
   normals = NULL;
}

void cfdContourBase::SetMapperInput( vtkPolyData* polydata )
{
   this->tris->SetInput( polydata );
   tris->GetOutput()->ReleaseDataFlagOn();

   // decimate points is used for lod control of contours
   this->deci->SetInput( tris->GetOutput() );
   this->deci->PreserveTopologyOn();
   this->deci->BoundaryVertexDeletionOff();
   deci->GetOutput()->ReleaseDataFlagOn();

   this->strip->SetInput( this->deci->GetOutput() );
   strip->GetOutput()->ReleaseDataFlagOn(); 

   if ( this->fillType == 0 )
   {
      normals->SetInput( this->strip->GetOutput() );
      normals->SetFeatureAngle( 130.0f );
      normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
      this->mapper->SetInput( normals->GetOutput() );
      mapper->ImmediateModeRenderingOn(); 
   }
   else if ( this->fillType == 1 )  // banded contours
   {
      // putting the decimation routines as inputs to the bfilter
      // cause the bfilter to crash while being updated
      this->bfilter->SetInput( polydata );
      double range[2];
      this->GetActiveDataSet()->GetUserRange( range );
      this->bfilter->GenerateValues( 10, range[0], range[1] );
      this->bfilter->SetScalarModeToValue();
      this->bfilter->GenerateContourEdgesOn();
      bfilter->GetOutput()->ReleaseDataFlagOn();
      normals->SetInput( bfilter->GetOutput() );
      normals->SetFeatureAngle( 130.0f );
      normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
      this->mapper->SetInput( normals->GetOutput() );
      mapper->ImmediateModeRenderingOn();
   }
   else if ( this->fillType == 2 )  // contourlines
   {
      this->cfilter->SetInput( this->strip->GetOutput() );
      double range[2];
      this->GetActiveDataSet()->GetUserRange( range );
      this->cfilter->GenerateValues( 10, range[0], range[1] );
      this->cfilter->UseScalarTreeOn();
      cfilter->GetOutput()->ReleaseDataFlagOn();
      normals->SetInput( cfilter->GetOutput() );
      normals->SetFeatureAngle( 130.0f );
      normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
      this->mapper->SetInput( this->normals->GetOutput() );
      mapper->ImmediateModeRenderingOn();    
   }
}

bool cfdContourBase::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because there is code in 
   // cfdObjects that doesn't belong there
   // Fix this
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_CONTOUR_FILL )
   {
      vprDEBUG(vesDBG,0) << "CHANGE_CONTOUR_FILL to type " 
                             << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
                             << std::endl << vprDEBUG_FLUSH;

      this->SetFillType( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
               == CHANGE_CONTOUR_SETTINGS )
   {  
      // warped contour settings
      double v[2];
      this->GetActiveDataSet()->GetUserRange( v );
      int scale = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN );
      this->warpedContourScale = (scale/50.0) * 0.2 
                    * this->GetActiveDataSet()->GetLength()/(float)(v[1]-v[0]);

      // contour lod control
      double lod = commandArray->GetCommandValue( cfdCommandArray::CFD_MAX );
      double realLOD = lod/100.0f;
      vprDEBUG(vesDBG,0) << "CHANGE_CONTOUR_SETTINGS LOD Settings" 
                             << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) << " : " << lod << " : " << realLOD
                             << std::endl << vprDEBUG_FLUSH;
      this->deci->SetTargetReduction( realLOD );
      return true;
   }
   return flag;
}

void cfdContourBase::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}

void cfdContourBase::SetFillType( const int type )
{
   if ( -1 < type && type < 3 )
      fillType = type;
   else
   {
      vprDEBUG(vesDBG, 0)
         << "cfdContourBase: requested fillType (" << type
         << ") is not available, using 0 instead"
         << std::endl << vprDEBUG_FLUSH;
      fillType = 0;
   }
}

