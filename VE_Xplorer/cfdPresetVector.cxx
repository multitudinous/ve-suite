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
 * File:          $RCSfile: cfdPresetVector.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPresetVector.h"
#include "cfdDataSet.h"
#include "cfdPlanes.h"
#include "cfdCuttingPlane.h"
#include <vpr/Util/Debug.h>

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

// this class requires that the dataset has a vector field.
cfdPresetVector::cfdPresetVector( const int xyz, const int numSteps )
{
   this->xyz = xyz;
   this->numSteps = numSteps;

   // set the cut function
   this->cutter = vtkCutter::New();

   // the pipeline for using nearest precomputed data is initialized below...
   this->PDactor = vtkActor::New();
}

cfdPresetVector::~cfdPresetVector()
{
   this->cutter->Delete();
   this->cutter = NULL;

   this->PDactor->Delete();
   this->PDactor = NULL;
}

void cfdPresetVector::Update( void )
{
   vprDEBUG(vprDBG_ALL, 1)  << "cfdPresetVector::ActiveDataSet = " 
                              << this->GetActiveDataSet() 
                              << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 1) << this->cursorType 
                           << " : " << usePreCalcData 
                           << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData )
   {
      vtkPolyData * preCalcData = this->GetActiveDataSet()
                              ->GetPrecomputedSlices( this->xyz )
                              ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vprDBG_ALL, 0) 
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

      this->PDactor->SetMapper( this->mapper );
      this->PDactor->GetProperty()->SetSpecularPower( 20.0f );
     
      vprDEBUG(vprDBG_ALL, 1)
         << "Yes Precalc : " << this->cursorType << " : " << usePreCalcData 
         << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      this->cuttingPlane = new cfdCuttingPlane( 
                  this->GetActiveDataSet()->GetDataSet()->GetBounds(),
                  xyz, numSteps );

      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
            this->GetActiveDataSet()->GetDataSet()->GetBounds() );
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

      vprDEBUG(vprDBG_ALL, 1) 
         << "No Precalc : " << this->cursorType << " : " << usePreCalcData
         << " : " << GetVectorRatioFactor() << std::endl << vprDEBUG_FLUSH;
   }
   this->updateFlag = true;
}

