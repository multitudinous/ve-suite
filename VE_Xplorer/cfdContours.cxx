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
 * File:          $RCSfile: cfdContours.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdContours.h"

#include "cfdDataSet.h"
#include "cfdPlanes.h"
#include "cfdEnum.h"    // needed for cursorType

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>

#include <vpr/Util/Debug.h>

cfdContours::cfdContours( const int xyz )
{
   this->xyz = xyz;

   if ( this->GetActiveMeshedVolume()->GetPrecomputedSlices( this->xyz )
        == NULL )
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdContours: planesData == NULL so returning" 
         << std::endl << vprDEBUG_FLUSH;
      return;
   }
}

cfdContours::~cfdContours()
{ 
}

void cfdContours::Update( void )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdContours::Update" 
                          << std::endl << vprDEBUG_FLUSH;

   //make sure that there are planesData and that the cursorType is correct...
   if ( this->mapper && this->cursorType == NONE )
   {   
    this->SetMapperInput( this->GetActiveMeshedVolume()
                        ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );

      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );

      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();

      this->updateFlag = true;
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdContours: !(mapper && cursorType == NONE)"
         << std::endl << vprDEBUG_FLUSH;

      this->updateFlag = false;
   }
}

