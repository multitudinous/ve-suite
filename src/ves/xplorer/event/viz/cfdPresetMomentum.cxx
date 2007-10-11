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
#include "VE_Xplorer/XplorerHandlers/cfdPresetMomentum.h"

#include "VE_Xplorer/XplorerHandlers/cfdCuttingPlane.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdPlanes.h"

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>
#include <vtkMultiGroupPolyDataMapper.h>

// the following is for the PD stuff...
#include <vtkActor.h>
#include <vtkProperty.h>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// this class requires that the dataset has a vector field.
cfdPresetMomentum::cfdPresetMomentum( const int xyz,
                                      int numSteps )
{  
   vprDEBUG(vesDBG, 1) << "cfdPresetMomentum::cfdPresetMomentum"
                           << std::endl << vprDEBUG_FLUSH;

   this->xyz = xyz;
   this->numSteps = numSteps;

   // set the cut function
//   this->cutter = vtkCutter::New();

   this->warper = vtkWarpVector::New();
}

cfdPresetMomentum::~cfdPresetMomentum()
{
   if(warper)
   {
      warper->Delete();
      warper = NULL;
   }
}

void cfdPresetMomentum::Update( void )
{
   vprDEBUG(vesDBG, 1) << "cfdPresetMomentum::Update, usePreCalcData = "
      << this->usePreCalcData << std::endl << vprDEBUG_FLUSH;
   
   if ( this->usePreCalcData )
   {
      vtkPolyData* preCalcData = this->GetActiveDataSet()
                                      ->GetPrecomputedSlices( xyz )
                                      ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vesDBG, 0) << "cfdPresetMomentum: no precalculated data"
                                 << std::endl << vprDEBUG_FLUSH;
         this->updateFlag = false;
         return;
      }

      this->warper->SetInput( preCalcData );
      this->warper->SetScaleFactor( this->warpedContourScale );
      //this->warper->Update();

      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
      //this->mapper->Update();
      vprDEBUG(vesDBG, 1)
         << "Yes Precalc : " << this->cursorType << " : " << usePreCalcData 
         << std::endl << vprDEBUG_FLUSH;
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
            this->GetActiveDataSet()->GetBounds(),
            this->xyz, numSteps );
 
     // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
            this->GetActiveDataSet()->GetBounds() );
      this->cuttingPlane->Advance( this->requestedValue );

      this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      this->cutter->Update();

      delete this->cuttingPlane;
      this->cuttingPlane = NULL;

      this->warper->SetInput( this->cutter->GetOutput() );
      this->warper->SetScaleFactor( this->warpedContourScale );
      //this->warper->Update();//can this go???
     
      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
      //this->mapper->Update();
*/
   }
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   //geodes.push_back( new VE_SceneGraph::Geode() );
   //geodes.back()->TranslateToGeode( temp );
   //temp->Delete();
   //this->updateFlag = true;
   try
   {   
		osg::ref_ptr< VE_SceneGraph::Geode > tempGeode = new VE_SceneGraph::Geode();
      tempGeode->TranslateToGeode( temp );
      geodes.push_back( tempGeode.get() ); 
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkMultiGroupPolyDataMapper::New();

      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdPresetMomentum " 
                           << std::endl << vprDEBUG_FLUSH;
   }
   //this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
   temp->Delete();
}
