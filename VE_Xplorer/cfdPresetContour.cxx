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
 * File:          $RCSfile: cfdPresetContour.cxx,v $
 * Date modified: $Date: 2004/04/30 19:30:49 $
 * Version:       $Revision: 1.20 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPresetContour.h"

#include "cfdCuttingPlane.h"
#include "cfdDataSet.h"
#include "cfdPlanes.h"

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkPolyDataMapper.h>

// the following is for the PD stuff...
#include <vtkActor.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

cfdPresetContour::cfdPresetContour( const int xyz, const int numSteps )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdPresetContour::cfdPresetMomentum"
                           << std::endl << vprDEBUG_FLUSH;
   this->xyz = xyz;

   this->cuttingPlane = new cfdCuttingPlane( 
            this->GetActiveMeshedVolume()->GetDataSet()->GetBounds(),
            this->xyz, numSteps );

   // set the cut function
   this->cutter = vtkCutter::New();
   this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );

   // the pipeline for using nearest precomputed data is initialized below...
   this->PDactor = vtkActor::New();
   this->PDactor->GetProperty()->SetSpecularPower( 20.0f );
}

cfdPresetContour::~cfdPresetContour()
{
   delete this->cuttingPlane;
   this->cuttingPlane = NULL;

   this->cutter->Delete();
   this->cutter = NULL;

   this->PDactor->Delete();
   this->PDactor = NULL;
}

void cfdPresetContour::Update( void )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdPresetContour::Update, usePreCalcData = "
      << this->usePreCalcData << std::endl << vprDEBUG_FLUSH;

   if ( this->usePreCalcData )
   {
      vtkPolyData * preCalcData = this->GetActiveMeshedVolume()
                                      ->GetPrecomputedSlices( this->xyz )
                                      ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vprDBG_ALL, 0) << "cfdPresetContour: no precalculated data"
                                 << std::endl << vprDEBUG_FLUSH;

         this->updateFlag = false;
         return;
      }

      this->SetMapperInput( preCalcData );
      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();

      this->PDactor->SetMapper( this->mapper );
   }
   else
   {
      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
                  this->GetActiveMeshedVolume()->GetDataSet()->GetBounds() );
      this->cuttingPlane->Advance( this->requestedValue );

      this->cutter->SetInput( this->GetActiveMeshedVolume()->GetDataSet() );
      this->cutter->Update();

      this->SetMapperInput( this->cutter->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();
   }
   this->updateFlag = true;
}
