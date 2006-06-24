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
 * File:          $RCSfile: cfdPresetContour.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdPresetContour.h"

#include "VE_Xplorer/cfdCuttingPlane.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdPlanes.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
#include "VE_Xplorer/SceneGraph/cfdGeode.h"

#include "VE_Xplorer/cfdRawNodeWriteTraverser.h"


#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyData.h>

// the following is for the PD stuff...
#include <vtkActor.h>
#include <vtkProperty.h>

#include "VE_Xplorer/cfdDebug.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdPresetContour::cfdPresetContour( const int xyz, const int numSteps )
:cfdContourBase()
{
   vprDEBUG(vesDBG, 1) << "cfdPresetContour::cfdPresetMomentum"
                           << std::endl << vprDEBUG_FLUSH;
   this->xyz = xyz;
   this->numSteps = numSteps;
   cuttingPlane = 0;
   // set the cut function
   this->cutter = vtkCutter::New();
   //this->polydata = vtkPolyData::New();
}

cfdPresetContour::~cfdPresetContour()
{
//   delete this->cuttingPlane;
   this->cuttingPlane = NULL;

   this->cutter->Delete();
   this->cutter = NULL;

   //this->polydata->Delete();
   //this->polydata = NULL;
}

void cfdPresetContour::Update( void )
{
   vprDEBUG(vesDBG,1) << "cfdPresetContour::Update, usePreCalcData = "
      << this->usePreCalcData << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData )
   {
      vtkPolyData * preCalcData = this->GetActiveDataSet()
                                      ->GetPrecomputedSlices( this->xyz )
                                      ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vesDBG, 0) << "cfdPresetContour: no precalculated data"
                                 << std::endl << vprDEBUG_FLUSH;

         this->updateFlag = false;
         return;
      }

      this->SetMapperInput( preCalcData );
      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
      //this->mapper->Update();
   }
   else
   {
      if ( cuttingPlane )
      {
         delete this->cuttingPlane;
         this->cuttingPlane = NULL;
      }

      CreatePlane();
/*
      this->cuttingPlane = new cfdCuttingPlane( 
            this->GetActiveDataSet()->GetDataSet()->GetBounds(),
            this->xyz, numSteps );

      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
                  this->GetActiveDataSet()->GetDataSet()->GetBounds() );
//std::cout<<"GETBOUNDS: "<<this->GetActiveDataSet()->GetDataSet()->GetBounds()<<std::endl;

      this->cuttingPlane->Advance( this->requestedValue );
      this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      this->cutter->Update();

      this->SetMapperInput( this->cutter->GetOutput() );
//      this->SetMapperInput( polydata );

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
*/
      //this->mapper->Update();
   }
   
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
//temp->GetMapper()->GetInput()->Print( std::cout );
//temp->GetMapper()->Update();
   //geodes.push_back( new VE_SceneGraph::cfdGeode() );
   //geodes.back()->TranslateTocfdGeode( temp );
   //temp->Delete();
   //this->updateFlag = true;

   try
   {
      VE_SceneGraph::cfdGeode* tempGeode = new VE_SceneGraph::cfdGeode();
      tempGeode->TranslateTocfdGeode( temp );
      geodes.push_back( tempGeode );
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkPolyDataMapper::New();

      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdPresetContour" 
                           << std::endl << vprDEBUG_FLUSH;
   }

   temp->Delete();

   if ( cuttingPlane )
   {
      delete this->cuttingPlane;
      this->cuttingPlane = NULL;
   }
}

//This function test for vtk data and if it is present creates the plane.
//If no vtk data is present for the plane selection, this function will 
//find the next closest plane.
void cfdPresetContour::CreatePlane( void )
{
      this->cuttingPlane = new cfdCuttingPlane( 
            this->GetActiveDataSet()->GetDataSet()->GetBounds(),
            this->xyz, numSteps );

      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
                  this->GetActiveDataSet()->GetDataSet()->GetBounds() );

      this->cuttingPlane->Advance( this->requestedValue );
      this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      this->cutter->Update();

      vtkPolyData* polydata = this->cutter->GetOutput();

      if( (polydata->GetNumberOfPoints()) < 1 || (polydata->GetNumberOfPolys()) < 1 ) 
      {
         std::cerr<<"No data for this plane : cfdPresetContour"<<std::endl;
         std::cerr<<"Finding next closest plane"<<std::endl;
         while ( (polydata->GetNumberOfPoints()) < 1 || (polydata->GetNumberOfPolys()) < 1 )  //&&(this->TargetReduction > 0.0) )
         {
            if( requestedValue < 50 )
            {
               this->requestedValue = this->requestedValue + 1;
            }
            else 
            {
               this->requestedValue = this->requestedValue - 1;
            }
            this->cuttingPlane->Advance( this->requestedValue ); 
            this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
            this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
            this->cutter->Update();
            polydata = this->cutter->GetOutput();            
         }    
      }       

      this->SetMapperInput( polydata );

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
}

