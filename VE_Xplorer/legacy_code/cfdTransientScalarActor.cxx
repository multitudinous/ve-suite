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
#include "cfdTransientScalarActor.h"
#include "cfdReadParam.h"
#include "cfdDataSet.h"

#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkDataSet.h>
#include <vtkGeometryFilter.h>
#include <vpr/Util/Debug.h>

void cfdTransientScalarActor::definePipeline()
{
   //this->minmax[0] = this->param->transientInfo[0]->min_max[ this->param->transientInfo[0]->activeScalar ][ 0 ];
   //this->minmax[1] = this->param->transientInfo[0]->min_max[ this->param->transientInfo[0]->activeScalar ][ 1 ];

   vtkGeometryFilter * geoFilter = vtkGeometryFilter::New();
   geoFilter->SetInput( this->dataset );
   geoFilter->Update();

   this->mapper->SetColorModeToMapScalars();
   this->mapper->SetInput( geoFilter->GetOutput() );
   geoFilter->Delete();
   
   if ( this->dataset->GetPointData()->GetScalars()->GetLookupTable() )
   {
      vprDEBUG(vprDBG_ALL,0)
         << " A lookup table is being read from the vtk file" 
         << std::endl << vprDEBUG_FLUSH;
      this->mapper->SetScalarRange( this->activeDataSet
                                        ->GetParent()->GetUserRange() );
      this->mapper->SetLookupTable( this->activeDataSet
                                        ->GetParent()->GetLookupTable() );
   }
   else
   {
      vprDEBUG(vprDBG_ALL,1) << " A blue-to-red lookup table is constructed" 
                             << std::endl << vprDEBUG_FLUSH;
/*
      this->mapper->SetScalarRange( this->minmax );
      vprDEBUG(vprDBG_ALL,0)
         << " A blue-to-red lookup table is constructed from scalar min max : " 
         << this->minmax[0] << " : " << this->minmax[1]
         << std::endl << vprDEBUG_FLUSH;
*/
      // Specify the lookup table
      vtkLookupTable * lut = vtkLookupTable::New();
      lut->SetNumberOfColors(256); //default is 256
      lut->SetHueRange(2.0f/3.0f, 0.0f); //a blue-to-red scale
      //lut->SetTableRange( this->minmax );
      lut->SetTableRange( this->activeDataSet->GetUserRange() );
      lut->Build();
      this->mapper->SetScalarRange( this->activeDataSet->GetParent()->GetUserRange() );
      this->mapper->SetLookupTable( lut );
      lut->Delete();
   }

   this->mapper->Update();
}

