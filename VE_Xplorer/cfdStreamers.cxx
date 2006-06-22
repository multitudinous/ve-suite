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
#include "VE_Xplorer/SceneGraph/cfdGeode.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointSet.h>
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

#include "VE_Xplorer/cfdDebug.h"

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
//   this->lineDiameter = 0.0f;
   this->lineDiameter = 1.0f;
   arrowDiameter = 1;
   streamArrows = 0;
   pointSource = 0;

//   command = 0;
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
      vprDEBUG(vesDBG,0) << "|\tcfdStreamers::Update, No Cursor Type Selected" << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vesDBG,0) << "|   cfdStreamers::Update, origin = "
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
      streamPoints->SetSource( this->pointSource );
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

   this->stream->SetSource( this->pointSource );
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
//   this->filter->SetInput( this->tubeFilter->GetOutput() );  

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
//      append->AddInput( tubeFilter->GetOutput() );  
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

//      this->mapper->SetInput( tubeFilter->GetOutput() );   
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
      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdStreamers " 
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

   vprDEBUG(vesDBG,0) << "|\tcfdStreamers::Update End" << std::endl << vprDEBUG_FLUSH;
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
   bool flag = false; //cfdObjects::CheckCommandId( commandArray );
   std::string commandType;
   if ( veCommand )
   {
      commandType = veCommand->GetCommandName();
   }
   else
   {
      commandType = "wait";
   }

   if ( !commandType.compare( "Streamline_Data" ) )   
   {
      VE_XML::DataValuePair* commandData = veCommand->GetDataValuePair( 0 );
      std::vector < long > commandIds;
      commandData->GetData( commandIds );
      std::string newCommand = commandData->GetDataName();

      if ( !newCommand.compare( "USE_LAST_STREAMLINE_SEEDPOINTS" ) )         
      {
         return true;
      }

      else if ( !newCommand.compare( "BACKWARD_INTEGRATION" ) )         
      {
         vprDEBUG(vesDBG,0) << " BACKWARD_INTEGRATION" 
                                << std::endl << vprDEBUG_FLUSH;

         this->SetIntegrationDirection( 2 );
         return true;  
      }
      else if ( !newCommand.compare( "FORWARD_INTEGRATION" ) )         
      {
         vprDEBUG(vesDBG,0) << " FORWARD_INTEGRATION"
                                << std::endl << vprDEBUG_FLUSH;

         this->SetIntegrationDirection( 1 );
         return true;  
      }
      else if ( !newCommand.compare( "TWO_DIRECTION_INTEGRATION" ) )         
      {
         vprDEBUG(vesDBG,0) << " TWO_DIRECTION_INTEGRATION" 
                                << std::endl << vprDEBUG_FLUSH;

         this->SetIntegrationDirection( 0 );
         return true; 
      }
      else if ( !newCommand.compare( "CHANGE_INT_STEP_LENGTH" ) )         
      {
         vprDEBUG(vesDBG,0) << " CHANGE_INT_STEP_LENGTH\t"
            << commandIds.at(4) 
            << std::endl << vprDEBUG_FLUSH;

         this->SetIntegrationStepLength( commandIds.at(4) );
         return true;
      }
      else if ( !newCommand.compare( "CHANGE_PROPAGATION_TIME" ) )         
      {
         vprDEBUG(vesDBG,0) << " CHANGE_PROPAGATION_TIME\t" 
            << commandIds.at(5) 
            << std::endl << vprDEBUG_FLUSH;

         this->SetPropagationTime( commandIds.at(5) );
         return true;  
      }
      else if ( !newCommand.compare( "CHANGE_STEP_LENGTH" ) )         
      {
         vprDEBUG(vesDBG,0) << " CHANGE_STEP_LENGTH\t" << commandIds.at(6) 
                                << std::endl << vprDEBUG_FLUSH;

         this->SetStepLength( commandIds.at(6) );
         return true;
      }
      else if ( !newCommand.compare( "STREAMLINE_ARROW" ) )         
      {
         vprDEBUG(vesDBG,0) << " STREAMLINE_ARROW\t" << this->cfdIso_value 
                                << std::endl << vprDEBUG_FLUSH;

         streamArrows = this->cfdIso_value;
         return true;
      }
      else if ( (!newCommand.compare( "STREAMLINE_DIAMETER" ) ) )// || (!newCommand.compare( "CHANGE_STREAMLINE_CURSOR" ) ) )         
      {
         vprDEBUG(vesDBG,0) << " STREAMLINE_DIAMETER\t" 
                                 << commandIds.at(7) 
                                 << std::endl << vprDEBUG_FLUSH;
 
         int diameter = commandIds.at(7);
         // this is to convert 1 to 50 on the GUI  to approx from 1 to 28 pixels
         //vector arrows and seed points are in feet
        // this->lineDiameter = exp(diameter*0.06666f);
         this->lineDiameter = 0.25 * diameter;

         vprDEBUG(vesDBG,1) << "       New Streamline Diameter : " 
                                << this->lineDiameter << std::endl << vprDEBUG_FLUSH;

         //this will make the arrows on the streamlines twice the diameter
         arrowDiameter = lineDiameter * 2.0f;
         return true;
      }
   }
   return flag;
}

void cfdStreamers::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   std::cerr << "doing nothing in cfdStreamers::UpdateCommand()" << std::endl;
}
