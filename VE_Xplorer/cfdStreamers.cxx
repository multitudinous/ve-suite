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
 * File:          $RCSfile: cfdStreamers.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdStreamers.h"
#include "cfdDataSet.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkRungeKutta4.h>
#include <vtkStreamLine.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>
#include <vtkStructuredGridReader.h>
#include <vtkStructuredGrid.h>
#include <vtkStructuredGridWriter.h>
#include <vpr/Util/Debug.h>

cfdStreamers::cfdStreamers( float diameter )
{
   this->stream = vtkStreamLine::New();
   this->integ = vtkRungeKutta4::New();


   this->tubeFilter = vtkTubeFilter::New();

   vprDEBUG(vprDBG_ALL,1) << "Default Streamline Diameter : " 
      << this->GetActiveMeshedVolume()->GetLength()*0.001f 
      << std::endl << vprDEBUG_FLUSH;

   if ( ! diameter )
   {
      diameter = this->GetActiveMeshedVolume()->GetLength()*0.001f;
   }
   this->tubeFilter->SetRadius( diameter );

   this->mapper = vtkPolyDataMapper::New();

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );

   this->integrationDirection = 0;
   this->propagationTime = 10.0f * this->GetActiveMeshedVolume()->GetMaxTime();
   this->integrationStepLength = 0.050f;
   this->stepLength = this->GetActiveMeshedVolume()->GetMeanCellLength()/30.0f;
}


cfdStreamers::~cfdStreamers()
{
   this->stream->Delete();
   this->tubeFilter->Delete();
   this->mapper->Delete();
   this->actor->Delete();
   this->integ->Delete();
}

void cfdStreamers::Update( void )
{
    
   vprDEBUG(vprDBG_ALL,0) << "|   cfdStreamers::Update, origin = "
      << this->origin[0] << " : " << this->origin[1] << " : " 
      << this->origin[2] << std::endl 
      << " Prop Time : " << this->propagationTime 
      << " Integration Step Length : " << this->integrationStepLength 
      << " Step Length : " << this->stepLength 
      << " Integration Direction : " << this->integrationDirection
      << std::endl <<
   vprDEBUG_FLUSH;
   //The Block below is a test by Yang
   this->stream->SetInput( (vtkDataSet*)this->GetActiveMeshedVolume()->GetDataSet() );

   
   //this->stream->SetSource( this->ptsglyph->GetOutput() );
   //this->stream->SetStartPosition( 0.0,0.0, 2.0 );
   
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
   //this->stream->SetStepLength( this->GetActiveMeshedVolume()->GetMeanCellLength() ); 

   if ( this->integrationDirection == 0 )
   {
      this->stream->SetIntegrationDirectionToIntegrateBothDirections();      
   }
   else if ( this->integrationDirection == 1 )
   {
      this->stream->SetIntegrationDirectionToForward();
   }
   else if ( this->integrationDirection == 2 )
   {
      this->stream->SetIntegrationDirectionToBackward();
   }
   this->stream->SetNumberOfThreads( 1 );


   this->stream->SetSource( (vtkDataSet*)this->pointSource );
   this->stream->SetIntegrator( this->integ );
   
   //this->stream->Update();

   //this->stream->DebugOn();
   //   this->stream->Print( cout );
   
   
   // Good Test code to see if you are actually getting streamlines
   //vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
   //writer->SetInput( ( vtkPolyData * ) stream->GetOutput() );
   //writer->SetFileName( "teststreamers.vtk" );
   //writer->Write();
   


   //this->tubeFilter->DebugOn();
   this->tubeFilter->SetInput( this->stream->GetOutput() );
   //   this->tubeFilter->Print( cout );
   this->tubeFilter->Update();
   //this->tubeFilter->DebugOn();


   //this->filter = vtkGeometryFilter::New();
   //this->filter->SetInput( this->tubeFilter->GetOutput() );
   
   this->mapper->SetInput( this->tubeFilter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();
   this->mapper->SetScalarRange( this->GetActiveMeshedVolume()->GetUserRange() );
   this->mapper->SetLookupTable( this->GetActiveMeshedVolume()->GetLookupTable() );
   //this->mapper->DebugOn();
   //this->tubeFilter->Print( cout );
   this->mapper->Update();
   //this->mapper->Print( cout );
 
   this->updateFlag = true;
   
   vprDEBUG(vprDBG_ALL,0) << "|   cfdStreamers::Update End" << std::endl << vprDEBUG_FLUSH;
}

vtkPolyData * cfdStreamers::GetStreamersOutput( void )
{
   return ( stream->GetOutput() );
}

void cfdStreamers::SetIntegrationDirection( int value )
{
   this->integrationDirection = value;
}

void cfdStreamers::SetPropagationTime( int value )
{
   this->propagationTime = (float)value * 
                  ( 10.0f * this->GetActiveMeshedVolume()->GetMaxTime() / 50.0f );
}

void cfdStreamers::SetIntegrationStepLength( int value )
{
   this->integrationStepLength = (float)value * ( 0.050f / 50.0f );
}

void cfdStreamers::SetStepLength( int value )
{
   this->stepLength = (float)value * ((this->GetActiveMeshedVolume()
                                             ->GetMeanCellLength()/30.0f) / 50.0f);     
}
