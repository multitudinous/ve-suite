
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/event/viz/cfdContourBase.h>
#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

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
#include <vtkCutter.h>
#include <vtkPlane.h>
#include <vtkCompositeDataPipeline.h>

#include <vtkMultiGroupPolyDataMapper.h>
#include <vtkMultiGroupDataGeometryFilter.h>
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
:cfdObjects()
{
   deci = vtkDecimatePro::New();
   
   //this->filter = vtkMultiGroupDataGeometryFilter::New();
   cfilter = vtkContourFilter::New();              // for contourlines
   bfilter = vtkBandedPolyDataContourFilter::New();// for banded contours
   // turn clipping on to avoid unnecessary value generations with 
   // vtkBandedPolyDataContourFilter::GenerateValues().
   bfilter->ClippingOn();   
   tris = vtkTriangleFilter::New();
   strip = vtkStripper::New();

   mapper = vtkMultiGroupPolyDataMapper::New();
   mapper->SetColorModeToMapScalars();
   mapper->ImmediateModeRenderingOn();
   normals = vtkPolyDataNormals::New();

   warpedContourScale = 0.0f;
   contourOpacity = 1.0f;
   contourLOD = 1; 
   cuttingPlane = 0;
}

cfdContourBase::~cfdContourBase()
{
   //vprDEBUG(vesDBG,2) << "cfdContourBase destructor"
    //                      << std::endl  << vprDEBUG_FLUSH;

   //this->filter->Delete();
   //this->filter = NULL;

   if(cfilter)
   {
      this->cfilter->Delete();
      this->cfilter = 0;
   }
   if(bfilter)
   {   
      this->bfilter->Delete();
      this->bfilter = 0;
   }

   if(tris)
   {   
      this->tris->Delete();
      this->tris = 0;
   }

   if(strip)
   { 
      this->strip->Delete();
      this->strip = 0;
   }

   if(mapper)
   {  
      this->mapper->Delete();
      this->mapper = 0;
   }   

   if(deci)
   {
      this->deci->Delete();
      this->deci = 0;
   }
   if(normals)
   {
      normals->Delete();
      normals = 0;
   }

   if(cuttingPlane)
   {
      delete cuttingPlane;
      cuttingPlane = NULL;
   }
}  

void cfdContourBase::SetMapperInput( vtkPolyData* polydata )
{
   this->tris->SetInput( polydata );
   tris->Update();
   tris->GetOutput()->ReleaseDataFlagOn();

   // decimate points is used for lod control of contours
   this->deci->SetInput( tris->GetOutput() );
   this->deci->PreserveTopologyOn();
   this->deci->BoundaryVertexDeletionOff();
   deci->Update();
   deci->GetOutput()->ReleaseDataFlagOn();

   this->strip->SetInput( this->deci->GetOutput() );
   strip->Update();
   strip->GetOutput()->ReleaseDataFlagOn(); 

   if ( this->fillType == 0 )
   {
      normals->SetInput( strip->GetOutput() );
      normals->SetFeatureAngle( 130.0f );
      //normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
      normals->Update();
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
      //normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
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
      //normals->GetOutput()->ReleaseDataFlagOn(); 
      normals->ComputePointNormalsOn();
      //normals->ComputeCellNormalsOn();
      normals->FlipNormalsOn();
   }
   mapper->SetInputConnection( normals->GetOutputPort() );
   mapper->ImmediateModeRenderingOn();    
}

void cfdContourBase::UpdateCommand()
{
   //Call base method - currently does nothing
   cfdObjects::UpdateCommand();

   //Extract the specific commands from the overall command
   ves::open::xml::DataValuePairWeakPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
   ves::open::xml::Command* objectCommand = dynamic_cast< ves::open::xml::Command* >( activeModelDVP->GetDataXMLObject() );

   //Extract the plane position
   activeModelDVP = objectCommand->GetDataValuePair( "Position" );
   double planePosition;
   activeModelDVP->GetData( planePosition );
   SetRequestedValue( static_cast< int >( planePosition ) );

   activeModelDVP = objectCommand->GetDataValuePair( "Plane Option" );
   if ( activeModelDVP )
   {
      std::string preCalculatedFlag;
      activeModelDVP->GetData( preCalculatedFlag );

      if( preCalculatedFlag == "Use Nearest Precomputed Plane" )
      {
         SetPreCalcFlag( true );
      }
   }
   else
   {
      SetPreCalcFlag( false );
   }

   //Extract the advanced settings from the commands
   activeModelDVP = objectCommand->GetDataValuePair( "Advanced Scalar Settings" );
   objectCommand = dynamic_cast< ves::open::xml::Command* >( activeModelDVP->GetDataXMLObject() );

   // set the opacity
   activeModelDVP = objectCommand->GetDataValuePair( "Contour Opacity" );
   double opacity;
   activeModelDVP->GetData( opacity );
   contourOpacity = opacity * 0.01f;

   // set the warped contour scale
   activeModelDVP = objectCommand->GetDataValuePair( "Warped Contour Scale" );
   double contourScale;
   activeModelDVP->GetData( contourScale );
   double v[2];
   this->GetActiveDataSet()->GetUserRange( v );
   int scale = contourScale;
   this->warpedContourScale = (scale/50.0) * 0.2 * 1.0f / (float)(v[1]-v[0]);

   // Set the lod values
   activeModelDVP = objectCommand->GetDataValuePair( "Contour LOD" );
   double contourLOD;
   activeModelDVP->GetData( contourLOD );
   double lod = contourLOD;
   double realLOD = lod * 0.01f;
   vprDEBUG(vesDBG,0) << "CHANGE_CONTOUR_SETTINGS LOD Settings" 
                          << contourLOD << " : " << lod << " : " << realLOD
                          << std::endl << vprDEBUG_FLUSH;
   this->deci->SetTargetReduction( realLOD );

   activeModelDVP = objectCommand->GetDataValuePair( "Type" );
   std::string contourType;
   activeModelDVP->GetData( contourType );
   
   if ( contourType == "Graduated" )
   {
      SetFillType( 0 );
   }
   else if ( contourType == "Banded" )
   {
      SetFillType( 1 );
   }
   else if ( contourType == "Lined" )
   {
      SetFillType( 2 );
   }
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
void cfdContourBase::CreatePlane( void )
{
   if(!cuttingPlane)
   {
      cuttingPlane = new cfdCuttingPlane( 
          GetActiveDataSet()->GetBounds(),
         xyz, numSteps );
   }
   

   // insure that we are using correct bounds for the given data set...
   cuttingPlane->SetBounds( 
         GetActiveDataSet()->GetBounds() );

   cuttingPlane->Advance( requestedValue );
   vtkCutter* tempCutter = vtkCutter::New();
   tempCutter->SetCutFunction( cuttingPlane->GetPlane() );
   tempCutter->SetInput(GetActiveDataSet()->GetDataSet());
   tempCutter->Update();
   vtkPolyData* tempPolydata = 0;
   tempPolydata = ApplyGeometryFilter(tempCutter->GetOutputPort());
   tempPolydata->Update();
   SetMapperInput( tempPolydata );

   mapper->SetScalarRange( GetActiveDataSet()
                                     ->GetUserRange() );
   mapper->SetLookupTable( GetActiveDataSet()
                                     ->GetLookupTable() );
   if(cuttingPlane)
   {
      delete cuttingPlane;
      cuttingPlane = NULL;
   }
   tempCutter->Delete();
}
