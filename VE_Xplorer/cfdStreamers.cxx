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
#include "cfdEnum.h"
#include "cfdCommandArray.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkRungeKutta4.h>
#include <vtkStreamLine.h>
#include <vtkTubeFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>

#include <vpr/Util/Debug.h>

cfdStreamers::cfdStreamers( void )
{
   this->stream = vtkStreamLine::New();
   this->integ = vtkRungeKutta4::New();

   this->tubeFilter = vtkTubeFilter::New();

   this->mapper = vtkPolyDataMapper::New();

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );

   this->integrationDirection = 0;
   this->propagationTime = -1;
   this->integrationStepLength = -1;
   this->stepLength = -1;
   this->lineDiameter = 0.0f;
}

cfdStreamers::~cfdStreamers()
{
   this->stream->Delete();
   this->integ->Delete();
   this->tubeFilter->Delete();
   this->mapper->Delete();
   this->actor->Delete();
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
      << std::endl << vprDEBUG_FLUSH;

   this->tubeFilter->SetRadius( this->lineDiameter );

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
   this->mapper->SetScalarRange( this->GetActiveDataSet()->GetUserRange() );
   this->mapper->SetLookupTable( this->GetActiveDataSet()->GetLookupTable() );
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
                  ( 100.0f * this->GetActiveDataSet()->GetMaxTime() / 50.0f );
}

void cfdStreamers::SetIntegrationStepLength( int value )
{
   this->integrationStepLength = (float)value * ( 0.050f / 50.0f );
}

void cfdStreamers::SetStepLength( int value )
{
   this->stepLength = (float)value * ((this->GetActiveDataSet()
                                           ->GetMeanCellLength()/30.0f) / 50.0f);     
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
      
      this->SetPropagationTime( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_INT_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_INT_STEP_LENGTH\t"
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
         << std::endl << vprDEBUG_FLUSH;
      
      this->SetIntegrationStepLength( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEP_LENGTH )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_STEP_LENGTH\t" << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                             << std::endl << vprDEBUG_FLUSH;
         
      this->SetStepLength( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STREAMLINE_DIAMETER )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_STEP_LENGTH\t" 
                              << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                              << std::endl << vprDEBUG_FLUSH;
         
      // diameter is obtained from gui, -100 < vectorScale < 100
      // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
      // convert range to -2.5 < x < 2.5, and compute the exponent...
      float range = 2.5f;
      int diameter = commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      this->lineDiameter = exp( diameter / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetLength()*0.001f;

         vprDEBUG(vprDBG_ALL,1) << "       New Streamline Diameter : " 
                             << this->lineDiameter << std::endl << vprDEBUG_FLUSH;
      return true;
   }
   return flag;
}

void cfdStreamers::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdStreamers::UpdateCommand()" << endl;
}

