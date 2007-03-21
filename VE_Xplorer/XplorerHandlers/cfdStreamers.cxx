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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdStreamers.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"

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

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

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
   //pointSource = 0;

   xValue = 4;
   yValue = 4;
   zValue = 4;
   seedPoints = 0;
   points = 0;
   xMinBB = 0;
   yMinBB = 0;
   zMinBB = 0;
   xMaxBB = 1;
   yMaxBB = 1;
   zMaxBB = 1;
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
   if ( seedPoints == NULL )
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
      streamPoints->SetInput( static_cast< vtkDataSet* >( this->GetActiveDataSet()->GetDataSet() ) );
      streamPoints->SetSource( seedPoints );
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

   this->stream->SetSource( seedPoints );
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
      cone->SetResolution( 5 );

      cones = vtkGlyph3D::New();
      cones->SetInput( streamPoints->GetOutput() );
      cones->SetSource( cone->GetOutput() );
      cones->SetScaleFactor( arrowDiameter );
      //cones->SetScaleModeToScaleByVector();
      cones->SetScaleModeToDataScalingOff();
      cones->SetVectorModeToUseVector();
      //cones->GetOutput()->ReleaseDataFlagOn();

      append = vtkAppendPolyData::New();
//      append->AddInput( tubeFilter->GetOutput() );  
      append->AddInput( stream->GetOutput() );
      append->AddInput( cones->GetOutput() );
      append->Update();
      //append->GetOutput()->ReleaseDataFlagOn();

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
      //normals->GetOutput()->ReleaseDataFlagOn();

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
		osg::ref_ptr< VE_SceneGraph::Geode > tempGeode = new VE_SceneGraph::Geode();
      tempGeode->TranslateToGeode( temp );
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

void cfdStreamers::SetPropagationTime( double value )
{
   this->propagationTime = value * 
                  ( 100.0f * this->GetActiveDataSet()->GetMaxTime() / 20.0f );
}
////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetIntegrationStepLength( int value )
{
   this->integrationStepLength = (float)value * ( 0.050f )/50.0f;
}
////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::SetStepLength( int value )
{
   this->stepLength = (float)value * ((this->GetActiveDataSet()
                                           ->GetMeanCellLength()/30.0f) /50.0f); // 
   
}
////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::UpdateCommand()
{
   //Call base method - currently does nothing
   cfdObjects::UpdateCommand();
   
   //Extract the specific commands from the overall command
   VE_XML::DataValuePair* activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
   VE_XML::Command* objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );
   
   //Extract the integration direction
   activeModelDVP = objectCommand->GetDataValuePair( "Integration Direction" );
   std::string intDirection;
   activeModelDVP->GetData( intDirection );
   
   if ( !intDirection.compare( "backward" ) )         
   {
      vprDEBUG(vesDBG,0) << " BACKWARD_INTEGRATION" 
      << std::endl << vprDEBUG_FLUSH;
      
      this->SetIntegrationDirection( 2 );
   }
   else if ( !intDirection.compare( "forward" ) )         
   {
      vprDEBUG(vesDBG,0) << " FORWARD_INTEGRATION"
      << std::endl << vprDEBUG_FLUSH;
      
      this->SetIntegrationDirection( 1 );
   }
   else if ( !intDirection.compare( "both directions" ) )         
   {
      vprDEBUG(vesDBG,0) << " TWO_DIRECTION_INTEGRATION" 
      << std::endl << vprDEBUG_FLUSH;
      
      this->SetIntegrationDirection( 0 );
   }
   
   //Extract the advanced settings from the commands
   activeModelDVP = objectCommand->GetDataValuePair( "Advanced Streamline Settings" );
   objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Use Stream Arrows" );
   unsigned int opacity;
   activeModelDVP->GetData( opacity );
   vprDEBUG(vesDBG,0) << " STREAMLINE_ARROW\t" << opacity 
      << std::endl << vprDEBUG_FLUSH;
   streamArrows = opacity;
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Step" );
   double stepSizeStream = 1.0f;
   activeModelDVP->GetData( stepSizeStream );
   vprDEBUG(vesDBG,0) << " CHANGE_STEP_LENGTH\t" << stepSizeStream 
      << std::endl << vprDEBUG_FLUSH;
   this->SetStepLength( static_cast< int >( stepSizeStream ) );
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Integration Step Size" );
   double contourLOD;
   activeModelDVP->GetData( contourLOD );
   vprDEBUG(vesDBG,0) << " CHANGE_INT_STEP_LENGTH\t"
      << contourLOD 
      << std::endl << vprDEBUG_FLUSH;
   this->SetIntegrationStepLength( static_cast< int >( contourLOD ) );
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Propagation Time" );
   double tempPropagationTime = 1.0f;
   activeModelDVP->GetData( tempPropagationTime );
   vprDEBUG(vesDBG,0) << " CHANGE_PROPAGATION_TIME\t" 
      << tempPropagationTime 
      << std::endl << vprDEBUG_FLUSH;
   this->SetPropagationTime( tempPropagationTime );
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Diameter" );
   double streamDiamter = 1.0f;
   activeModelDVP->GetData( streamDiamter );
   vprDEBUG(vesDBG,0) << " STREAMLINE_DIAMETER\t" 
      << streamDiamter 
      << std::endl << vprDEBUG_FLUSH;   
   // diameter is obtained from gui, -100 < vectorScale < 100
   // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
   // convert range to -2.5 < x < 2.5, and compute the exponent...
   float range = 2.5f;
   int diameter = static_cast< int >( streamDiamter );
   float localLineDiameter = exp( diameter / ( 100.0 / range ) ) * 1.0f *0.001f;
   
   // this is to normalize -100 to 100 on the GUI  to  1-21 for diameters
   // note that multiplying by 0.005 is the same as dividing by 200, or the range
   this->lineDiameter = (diameter + 110) * 0.005 *  20;
   
   vprDEBUG(vesDBG,1) << "|\tNew Streamline Diameter : " 
      << this->lineDiameter << std::endl << vprDEBUG_FLUSH;
   arrowDiameter = localLineDiameter * 60.0f;
   vprDEBUG(vesDBG,1) << "|\tNew Arrow Diameter : " 
      << arrowDiameter << std::endl << vprDEBUG_FLUSH;
   
   /////////////////////
   //activeModelDVP = objectCommand->GetDataValuePair( "Sphere/Arrow/Particle Size" );
   //double sphereArrow = 1.0f;
   //activeModelDVP->GetData( streamDiamter );
   
   /////////////////////
   activeModelDVP = objectCommand->GetDataValuePair( "Use Last Seed Pt" );
   unsigned int lastSeedPt;
   activeModelDVP->GetData( lastSeedPt );
   
   ////////////////////
   //Set the number of seed points in each direction and get the %BB info
   //Extract the advanced settings from the commands
   //activeModelDVP = objectCommand->GetDataValuePair( "Seed_Point_Settings" );
   //objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );
   activeModelDVP = objectCommand->GetDataValuePair( "Max_X_BB" );
   //if 1 is there then they all are there
   if ( activeModelDVP )
   {
      activeModelDVP->GetData( xMaxBB );   
      vprDEBUG(vesDBG,0) << "|\txMaxBB : " << xMaxBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Max_Y_BB" );
      activeModelDVP->GetData( yMaxBB );   
      vprDEBUG(vesDBG,0) << "|\tyMaxBB : " << yMaxBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Max_Z_BB" );
      activeModelDVP->GetData( zMaxBB );   
      vprDEBUG(vesDBG,0) << "|\tzMaxBB : " << zMaxBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Min_X_BB" );
      activeModelDVP->GetData( xMinBB );   
      vprDEBUG(vesDBG,0) << "|\txMinBB : " << xMinBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Min_Y_BB" );
      activeModelDVP->GetData( yMinBB );   
      vprDEBUG(vesDBG,0) << "|\tyMinBB : " << yMinBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Min_Z_BB" );
      activeModelDVP->GetData( zMinBB );   
      vprDEBUG(vesDBG,0) << "|\tzMinBB : " << zMinBB << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Num_X_Points" );
      activeModelDVP->GetData( xValue );   
      vprDEBUG(vesDBG,0) << "|\tX Points : " << xValue << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Num_Y_Points" );
      activeModelDVP->GetData( yValue );   
      vprDEBUG(vesDBG,0) << "|\tY Points : " << yValue << std::endl << vprDEBUG_FLUSH;
      activeModelDVP = objectCommand->GetDataValuePair( "Num_Z_Points" );
      activeModelDVP->GetData( zValue );
      vprDEBUG(vesDBG,0) << "|\tZ Points : " << zValue << std::endl << vprDEBUG_FLUSH;
   }
   CreateSeedPoints();
}
////////////////////////////////////////////////////////////////////////////////
void cfdStreamers::CreateSeedPoints( void )
{
   double bounds[ 6 ];
   GetActiveDataSet()->GetDataSet()->GetBounds(bounds);
   
   double xDiff = bounds[1] - bounds[0];
   double yDiff = bounds[3] - bounds[2];
   double zDiff = bounds[5] - bounds[4];
   
   double xMin = bounds[0] + (xDiff * xMinBB);
   double xMax = bounds[0] + (xDiff * xMaxBB);
   double yMin = bounds[2] + (yDiff * yMinBB);
   double yMax = bounds[2] + (yDiff * yMaxBB);
   double zMin = bounds[4] + (zDiff * zMinBB);
   double zMax = bounds[4] + (zDiff * zMaxBB);
   
   double xLoc = 0;
   double yLoc = 0;
   double zLoc = 0;
   int number = 0;
   //insert evenly spaced points inside bounding box
   if ( points )
   {      
      points->Delete();
   }
   points = vtkPoints::New();
   double deltaX = (xValue==1)?0:(xMax-xMin)/double(xValue-1);
   double deltaY = (yValue==1)?0:(yMax-yMin)/double(yValue-1);
   double deltaZ = (zValue==1)?0:(zMax-zMin)/double(zValue-1);
   
   for (unsigned int i = 0; i < xValue; ++i)
	{
      xLoc = xMin + (i*deltaX);
      for (unsigned int j = 0; j < yValue; ++j)
      {
         yLoc = yMin + (j*deltaY);
         for(unsigned int k = 0; k < zValue; k++)			
			{
            //points added in ptMin + length*iteration/(number of equal segments)
            //where (number of equal segments) = ptValue+1
            zLoc = zMin + (k*deltaZ);
            //std::cout << xLoc << " " <<  yLoc << " " <<  zLoc << std::endl;
            points->InsertPoint( number, xLoc, yLoc, zLoc ); 
            number=number+1;
			}
		}
	}
   
   //create polydata to be glyphed
   if ( seedPoints )
   {
      seedPoints->Delete();
   }
   seedPoints = vtkPolyData::New();
   seedPoints->SetPoints(points);
}
