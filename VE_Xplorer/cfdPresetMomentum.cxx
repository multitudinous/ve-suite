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
 * File:          $RCSfile: cfdPresetMomentum.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdPresetMomentum.h"

#include "cfdCuttingPlane.h"
#include "cfdDataSet.h"
#include "cfdEnum.h"
#include "cfdPlanes.h"

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>

// the following is for the PD stuff...
#include <vtkActor.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

// this class requires that the dataset has a vector field.
cfdPresetMomentum::cfdPresetMomentum( const int xyz, const float scale,
                                      int numSteps )
{  
   vprDEBUG(vprDBG_ALL, 1) << "cfdPresetMomentum::cfdPresetMomentum"
                           << std::endl << vprDEBUG_FLUSH;

   this->xyz = xyz;
   this->scale = scale;

   this->cuttingPlane = new cfdCuttingPlane( 
            this->GetActiveMeshedVolume()->GetDataSet()->GetBounds(),
            this->xyz, numSteps );

   // set the cut function
   this->cutter = vtkCutter::New();
   this->cutter->SetCutFunction( this->cuttingPlane->GetPlane() );

   this->warper = vtkWarpVector::New();
   this->warper->SetScaleFactor( this->GetScaleFactor() );

   // the pipeline for using nearest precomputed data is initialized below...
   this->PDactor = vtkActor::New();
   this->PDactor->GetProperty()->SetSpecularPower( 20.0f );
}

cfdPresetMomentum::~cfdPresetMomentum()
{
   delete this->cuttingPlane;
   this->cuttingPlane = NULL;

   this->cutter->Delete();
   this->cutter = NULL;

   this->warper->Delete();
   this->warper = NULL;

   this->PDactor->Delete();
   this->PDactor = NULL;
}

void cfdPresetMomentum::Update( void )
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdPresetMomentum::Update, usePreCalcData = "
      << this->usePreCalcData << std::endl << vprDEBUG_FLUSH;
   
   if ( this->usePreCalcData )
   {
      vtkPolyData * preCalcData = this->GetActiveMeshedVolume()
                                      ->GetPrecomputedSlices( xyz )
                                      ->GetClosestPlane( this->requestedValue );

      if ( preCalcData == NULL )
      {
         vprDEBUG(vprDBG_ALL, 0) << "cfdPresetMomentum: no precalculated data"
                                 << std::endl << vprDEBUG_FLUSH;
         this->updateFlag = false;
         return;
      }

      this->warper->SetInput( preCalcData );
      this->warper->SetScaleFactor( this->GetScaleFactor() );
      this->warper->Update();

      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();

      this->PDactor->SetMapper( this->mapper );
     
      vprDEBUG(vprDBG_ALL, 1)
         << "Yes Precalc : " << this->cursorType << " : " << usePreCalcData 
         << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      // insure that we are using correct bounds for the given data set...
      this->cuttingPlane->SetBounds( 
            this->GetActiveMeshedVolume()->GetDataSet()->GetBounds() );
      this->cuttingPlane->Advance( this->requestedValue );

      this->cutter->SetInput( this->GetActiveMeshedVolume()->GetDataSet() );
      this->cutter->Update();

      this->warper->SetInput( this->cutter->GetOutput() );
      this->warper->SetScaleFactor( this->GetScaleFactor() );
      this->warper->Update();//can this go???
     
      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();
   }
   this->updateFlag = true;
}

float cfdPresetMomentum::GetScaleFactor( )
{
   // the idea here was to properly size the size of the warp based on the data set. Good idea, but this implementation used scalar range when it should use vector magnitude range. This will work if the scalar is velocity magnitude.  
   // If you fix it, then mirror in cfdMomentums.cxx.
   double v[2];
   this->GetActiveMeshedVolume()->GetUserRange( v );

   float scaleFactor;
   if ( v[0] == v[1] )
   {
      scaleFactor = 1.0;
   }
   else
   {
      scaleFactor = this->scale * 0.2 
                    * this->GetActiveMeshedVolume()->GetLength()/(float)(v[1]-v[0]);
   }

   vprDEBUG(vprDBG_ALL, 1) << "scaleFactor = " << this->scale
                           << std::endl << vprDEBUG_FLUSH;
   return scaleFactor;
}

