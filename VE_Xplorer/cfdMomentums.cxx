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
 * File:          $RCSfile: cfdMomentums.cxx,v $
 * Date modified: $Date: 2004/03/23 16:29:16 $
 * Version:       $Revision: 1.14 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdMomentums.h"
#include "cfdDataSet.h"
#include "cfdEnum.h"    // needed for cursorType
#include "cfdPlanes.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>

#include <vpr/Util/Debug.h>

cfdMomentums::cfdMomentums( const int xyz, const float scale )
{
   this->xyz = xyz;
   this->scale = scale;

   this->warper = NULL;

   if ( this->GetActiveMeshedVolume()->GetPrecomputedSlices( this->xyz ) == NULL )
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdMomentums: planesData == NULL so returning"
         << std::endl << vprDEBUG_FLUSH;

      return;
   }

   this->warper = vtkWarpVector::New();
   this->warper->SetInput( this->GetActiveMeshedVolume()
                        ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );
   this->warper->SetScaleFactor( this->GetScaleFactor() );
}


cfdMomentums::~cfdMomentums()
{ 
   if ( this->warper != NULL )
   {
      this->warper->Delete();
      this->warper = NULL;
   }
}


void cfdMomentums::Update( void )
{
   //make sure that there are planesData and that the cursorType is correct...
   if ( this->mapper && this->cursorType == NONE )
   {
      this->warper->SetInput( this->GetActiveMeshedVolume()
                       ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );
      this->warper->SetScaleFactor( this->GetScaleFactor() );
      this->warper->Update();//can this go???

      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();//can this go???

      this->updateFlag = true;
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdMomentums::Update: !(mapper && cursorType == NONE)"
         << std::endl << vprDEBUG_FLUSH;

      this->updateFlag = false;
   }
}

float cfdMomentums::GetScaleFactor( )
{
   // the idea here was to properly size the size of the warp based on the data set. Good idea, but this implementation used scalar range when it should use vector magnitude range. This will work if the scalar is velocity magnitude.  
   // If you fix it, then mirror in cfdPresetMomentum.cxx.
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

