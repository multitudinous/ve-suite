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
 * File:          $RCSfile: cfdStreamers.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdStreamers.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_SceneGraph/cfdGeode.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkRungeKutta45.h>
#include <vtkStreamLine.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>
#include <vtkConeSource.h>
#include <vtkStreamPoints.h>
#include <vtkGlyph3D.h>
#include <vtkAppendPolyData.h>
#include <vtkStreamTracer.h>
#include <vtkStripper.h>
#include <vtkTriangleFilter.h>
#include <vtkPolyDataNormals.h>

#include <vpr/Util/Debug.h>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdStreamers::cfdStreamers( void )
{
   this->stream = vtkStreamLine::New();
   this->integ = vtkRungeKutta45::New();

   this->tubeFilter = vtkTubeFilter::New();
   this->mapper = vtkPolyDataMapper::New();

   this->integrationDirection = 0;
   this->propagationTime = -1;
   this->integrationStepLength = -1;
   this->stepLength = -1;
   this->lineDiameter = 0.0f;
   arrowDiameter = 1;
   streamArrows = 0;
}

cfdStreamers::~cfdStreamers()
{
   this->stream->Delete();
   this->integ->Delete();
   this->tubeFilter->Delete();
   this->mapper->Delete();
}

void cfdStreamers::Update( void )
{
   if ( pointSource == NULL )
   {
      vprDEBUG(vprDBG_ALL,0) << "|\tcfdStreamers::Update, No Cursor Type Selected" << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vprDBG_ALL,0) << "|   cfdStreamers::Update, origin = "
      << this->origin[0] << " : " << this->origin[1] << " : " 
      << this->origin[2] << std::endl 
      << " Prop Time : " << this->propagationTime 
      << " Integration Step Length : " << this->integrationStepLength 
      << " Step Length : " << this->stepLength 
      << " Integration Direction : " << this->integrationDirection
      << std::endl << vprDEBUG_FLUSH;
/*
//Create source for streamtubes
vtkStreamTracer streamer
streamer SetInput [reader GetOutput]
streamer SetStartPosition 0.1 2.1 0.5
streamer SetMaximumPropagation 0 500
streamer SetMinimumIntegrationStep 1 0.1
streamer SetMaximumIntegrationStep 1 1.0
streamer SetInitialIntegrationStep 2 0.2
streamer SetIntegrationDirection 0
streamer SetIntegrator rk
streamer SetRotationScale 0.5
streamer SetMaximumError 1.0E-8

vtkAssignAttribute aa
aa SetInput [streamer GetOutput]
aa Assign Normals NORMALS POINT_DATA
*/
   this->tubeFilter->SetRadius( this->lineDiameter );
   this->tubeFilter->SetNumberOfSides( 3 );
   this->tubeFilter->SidesShareVerticesOn();
   

   if ( propagationTime == -1 )
   {
      this->propagationTime = 10.0f * this->GetActiveDataSet()->GetMaxTime();
   }
   
   if ( integrationStepLength == -1 )
   {
      this->integrationStepLength = 0.050f;
   }
   
   if ( stepLength == -1 )
   {
      this->stepLength = this->GetActiveDataSet()->GetMeanCellLength()/30.0f;
   }

   //The Block below is a test by Yang
   this->stream->SetInput( (vtkDataSet*)this->GetActiveDataSet()->GetDataSet() );

   //overall length of streamline
   this->stream->SetMaximumPropagationTime( this->propagationTime );
   //this->stream->SetMaximumPropagationTime( 5000 );  
   //this->stream->SetMaximumPropagationTime( 30 );  
   
   // typically < 1
   //this->stream->SetIntegrationStepLength( 0.01 );
   this->stream->SetIntegrationStepLength( this->integrationStepLength );    

   // length of line segments < maxPropTime
   //this->stream->SetIntegrationStepLength( 0.1 );    
   //this->stream->SetStepLength( 0.001 );
   this->stream->SetStepLength( this->stepLength );

   // Stream Points Section
   vtkStreamPoints* streamPoints = 0;
   vtkConeSource* cone = 0;
   vtkGlyph3D* cones = 0;
   vtkAppendPolyData* append = 0;
   vtkPolyDataNormals* normals = 0;

   if ( streamArrows )
   {
      streamPoints = vtkStreamPoints::New();
      streamPoints->SetInput( (vtkDataSet*)this->GetActiveDataSet()->GetDataSet() );
      streamPoints->SetSource( (vtkDataSet*)this->pointSource );
      streamPoints->SetTimeIncrement( this->stepLength * 500 );
      streamPoints->SetMaximumPropagationTime( this->propagationTime );
      streamPoints->SetIntegrationStepLength( this->integrationStepLength );    
      streamPoints->SetIntegrator( this->integ );
      streamPoints->SpeedScalarsOff();
   }

   if ( this->integrationDirection == 0 )
   {
      this->stream->SetIntegrationDirectionToIntegrateBothDirections();
      if ( streamArrows )
         streamPoints->SetIntegrationDirectionToIntegrateBothDirections();
   }
   else if ( this->integrationDirection == 1 )
   {
      this->stream->SetIntegrationDirectionToForward();
      if ( streamArrows )
         streamPoints->SetIntegrationDirectionToForward();
   }
   else if ( this->integrationDirection == 2 )
   {
      this->stream->SetIntegrationDirectionToBackward();
      if ( streamArrows )
         streamPoints->SetIntegrationDirectionToBackward();
   }
   this->stream->SetNumberOfThreads( 1 );

   this->stream->SetSource( dynamic_cast< vtkDataSet* >( this->pointSource ) );
   this->stream->SetIntegrator( this->integ );
   //stream->GetOutput()->ReleaseDataFlagOn();
   
   // Good Test code to see if you are actually getting streamlines
   /*vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
   writer->SetInput( ( vtkPolyData * ) stream->GetOutput() );
   writer->SetFileName( "teststreamers.vtk" );
   writer->Write();*/
/*
   this->tubeFilter->SetInput( this->stream->GetOutput() );
   tubeFilter->GetOutput()->ReleaseDataFlagOn();
*/
   //this->filter = vtkGeometryFilter::New();
   //this->filter->SetInput( this->tubeFilter->GetOutput() );

   //vtkTriangleFilter *tris = vtkTriangleFilter::New();
   //vtkStripper *strip = vtkStripper::New();

   if ( streamArrows )
   {
      cone = vtkConeSource::New();
      cone->SetResolution( 3 );

      cones = vtkGlyph3D::New();
      cones->SetInput( streamPoints->GetOutput() );
      cones->SetSource( cone->GetOutput() );
      cones->SetScaleFactor( arrowDiameter );
      //cones->SetScaleModeToScaleByVector();
      cones->SetScaleModeToDataScalingOff();
      cones->SetVectorModeToUseVector();
      cones->GetOutput()->ReleaseDataFlagOn();

      append = vtkAppendPolyData::New();
      //append->AddInput( tubeFilter->GetOutput() );
      append->AddInput( stream->GetOutput() );
      append->AddInput( cones->GetOutput() );
      append->GetOutput()->ReleaseDataFlagOn();

      /*tris->SetInput( append->GetOutput() );
      tris->GetOutput()->ReleaseDataFlagOn();  
      strip->SetInput( tris->GetOutput() );
      strip->GetOutput()->ReleaseDataFlagOn();*/

      normals = vtkPolyDataNormals::New();
      normals->SetInput( append->GetOutput() );
      normals->SplittingOff();
      normals->ConsistencyOn();
      normals->AutoOrientNormalsOn();
      normals->ComputePointNormalsOn();
      normals->ComputeCellNormalsOff();
      normals->NonManifoldTraversalOff();
      normals->GetOutput()->ReleaseDataFlagOn();

      this->mapper->SetInput( normals->GetOutput() );
   }
   else
   {
      /*tris->SetInput( stream->GetOutput() );
      tris->GetOutput()->ReleaseDataFlagOn();  
      strip->SetInput(tris->GetOutput());
      strip->GetOutput()->ReleaseDataFlagOn();*/

      //this->mapper->SetInput( tubeFilter->GetOutput() );
      this->mapper->SetInput( stream->GetOutput() );
   }


   this->mapper->SetColorModeToMapScalars();
   this->mapper->SetScalarRange( this->GetActiveDataSet()->GetUserRange() );
   this->mapper->SetLookupTable( this->GetActiveDataSet()->GetLookupTable() );
   this->mapper->ImmediateModeRenderingOn();
 
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   temp->GetProperty()->SetLineWidth(lineDiameter);
   temp->GetProperty()->SetRepresentationToWireframe();
   //test to see if there is enough memory, if not, filters are deleted
   try
   {
      VE_SceneGraph::cfdGeode* tempGeode = new VE_SceneGraph::cfdGeode();
      tempGeode->TranslateTocfdGeode( temp );
      geodes.push_back( tempGeode ); 
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      stream->Delete();
      stream = vtkStreamLine::New();
      tubeFilter->Delete();
      tubeFilter = vtkTubeFilter::New();
      mapper->Delete();
      mapper = vtkPolyDataMapper::New();
      vprDEBUG(vprDBG_ALL,0) << "|\tMemory allocation failure : cfdStreamers " 
                              << std::endl << vprDEBUG_FLUSH;
   }
    
   temp->Delete();

   if ( streamArrows )
   {
      // Clean Up Now...
      streamPoints->Delete();
      append->Delete();
      cone->Delete();
      cones->Delete();
      normals->Delete();
   }

   vprDEBUG(vprDBG_ALL,0) << "|\tcfdStreamers::Update End" << std::endl << vprDEBUG_FLUSH;
}

vtkPolyData * cfdStreamers::GetStreamersOutput( void )
{
   // may need to gaurd this somehow
   return ( stream->GetOutput() );
}

void cfdStreamers::SetIntegrationDirection( int value )
{
   this->integrationDirection = value;
}

void cfdStreamers::SetPropagationTime( int value )
{
   this->propagationTime = (float)value * 
                  ( 100.0f * this->GetActiveDataSet()->GetMaxTime() / 20.0f );
}

void cfdStreamers::SetIntegrationStepLength( int value )
{
   this->integrationStepLength = (float)value * ( 0.050f )/50.0f;
}

void cfdStreamers::SetStepLength( int value )
{
   this->stepLength = (float)value * ((this->GetActiveDataSet()
                                           ->GetMeanCellLength()/30.0f) /50.0f); //     
}

bool cfdStreamers::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == USE_LAST_STREAMLINE_SEEDPOINTS )
   {
      // Need to fix this
      // look in old cfdApp to see how it was used
      //this->useLastSource = commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == BACKWARD_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " BACKWARD_INTEGRATION" 
                             << std::endl << vprDEBUG_FLUSH;

      this->SetIntegrationDirection( 2 );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == FORWARD_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " FORWARD_INTEGRATION"
                             << std::endl << vprDEBUG_FLUSH;

      this->SetIntegrationDirection( 1 );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TWO_DIRECTION_INTEGRATION )
   {
      vprDEBUG(vprDBG_ALL,0) << " 2_DIRECTION_INTEGRATION" 
                             << std::endl << vprDEBUG_FLUSH;

      this->SetIntegrationDirection( 0 );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_PROPAGATION_TIME )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_PROPAGATION_TIME\t" 
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;
      
      this->SetPropagationTime( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_INT_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_INT_STEP_LENGTH\t"
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
         << std::endl << vprDEBUG_FLUSH;
      
      this->SetIntegrationStepLength( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_STEP_LENGTH\t" << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                             << std::endl << vprDEBUG_FLUSH;
         
      this->SetStepLength( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STREAMLINE_ARROW )
   {
      vprDEBUG(vprDBG_ALL,0) << " STREAMLINE_ARROW\t" << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                             << std::endl << vprDEBUG_FLUSH;
         
      streamArrows = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STREAMLINE_DIAMETER )
   {
      vprDEBUG(vprDBG_ALL,0) << " STREAMLINE_DIAMETER\t" 
                              << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                              << std::endl << vprDEBUG_FLUSH;
         
      // diameter is obtained from gui, -100 < vectorScale < 100
      // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
      // convert range to -2.5 < x < 2.5, and compute the exponent...
      float range = 2.5f;
      int diameter = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      float localLineDiameter = exp( diameter / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetLength()*0.001f;

      // this is to normalize -100 to 100 on the GUI  to  1-21 for diameters
      // note that multiplying by 0.005 is the same as dividing by 200, or the range
      this->lineDiameter = (diameter + 110) * 0.005 *  20;

      vprDEBUG(vprDBG_ALL,1) << "       New Streamline Diameter : " 
                             << this->lineDiameter << std::endl << vprDEBUG_FLUSH;
      arrowDiameter = localLineDiameter * 4.0f;
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STREAMLINE_CURSOR )
   {
      // diameter is obtained from gui, -100 < vectorScale < 100
      // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
      // convert range to -2.5 < x < 2.5, and compute the exponent...
      float range = 2.5f;
      int diameter = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      arrowDiameter = 4.0f * exp( diameter / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetLength()*0.001f;
      return true;
   }
   
   return flag;
}

void cfdStreamers::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   std::cerr << "doing nothing in cfdStreamers::UpdateCommand()" << std::endl;
}

