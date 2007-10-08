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
#include "VE_Xplorer/XplorerHandlers/cfdPresetVector.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdPlanes.h"
#include "VE_Xplorer/XplorerHandlers/cfdCuttingPlane.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// this class requires that the dataset has a vector field.
cfdPresetVector::cfdPresetVector( const int xyz, const int numSteps )
{
   this->xyz = xyz;
   this->numSteps = numSteps;

   // set the cut function
   this->cutter = vtkCutter::New();
}

cfdPresetVector::~cfdPresetVector()
{
   this->cutter->Delete();
   this->cutter = NULL;
}

void cfdPresetVector::Update( void )
{
   vprDEBUG(vesDBG,1) << "cfdPresetVector::ActiveDataSet = " 
                          << this->GetActiveDataSet() 
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vesDBG,1) << this->cursorType 
                          << " : " << usePreCalcData 
                          << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData )
   {

      vtkPolyData * preCalcData = this->GetActiveDataSet()
                              ->GetPrecomputedSlices( this->xyz )
                              ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vesDBG, 0) 
            << "cfdPresetVector: no precalculated data available"
            << std::endl << vprDEBUG_FLUSH;
         this->updateFlag = false;
         return;
      }

      // get every nth point from the dataSet data
      this->ptmask->SetInput( preCalcData );
      this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
      this->ptmask->Update();

      this->SetGlyphWithThreshold();
      this->SetGlyphAttributes();
      this->glyph->Update();
      //this->glyph->DebugOn();

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                          ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                          ->GetLookupTable() );
      this->mapper->Update();
      vprDEBUG(vesDBG, 1)
         << "Yes Precalc : " << this->cursorType << " : " << usePreCalcData 
         << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      this->cuttingPlane = new cfdCuttingPlane( 
                  this->GetActiveDataSet()->GetBounds(),
                  xyz, numSteps );

      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
            this->GetActiveDataSet()->GetBounds() );
      this->cuttingPlane->Advance( this->requestedValue );

      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );
      this->cutter->Update();

      delete this->cuttingPlane;
      this->cuttingPlane = NULL;

      // get every nth point from the dataSet data
      this->ptmask->SetInput( this->cutter->GetOutput() );
      this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
      this->ptmask->Update();      

      this->SetGlyphWithThreshold();
      this->SetGlyphAttributes();
      this->glyph->Update();
      //this->glyph->Print( cout );

      this->filter->Update();

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
      this->mapper->Update();

      vprDEBUG(vesDBG, 1) 
         << "No Precalc : " << this->cursorType << " : " << usePreCalcData
         << " : " << GetVectorRatioFactor() << std::endl << vprDEBUG_FLUSH;
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
		osg::ref_ptr<VE_SceneGraph::Geode > tempGeode = new VE_SceneGraph::Geode();
      tempGeode->TranslateToGeode( temp );
      geodes.push_back( tempGeode ); 
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkPolyDataMapper::New();
      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdPresetVectors " 
                           << std::endl << vprDEBUG_FLUSH;
   }
   //this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
   temp->Delete();
}

