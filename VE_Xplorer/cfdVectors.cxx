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
 * File:          $RCSfile: cfdVectors.cxx,v $
 * Date modified: $Date: 2004/03/23 16:29:19 $
 * Version:       $Revision: 1.24 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdVectors.h"
#include "cfdDataSet.h"
#include "cfdPlanes.h"
#include "cfdEnum.h"

#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkPolyDataWriter.h>

#include <vpr/Util/Debug.h>

cfdVectors::cfdVectors( const int xyz )
{
   this->xyz = xyz;

   if ( this->GetActiveMeshedVolume()->GetPrecomputedSlices( this->xyz )
                                     ->GetPlanesData() == NULL )
   {
      vprDEBUG(vprDBG_ALL, 0) 
         << "cfdVectors, planesData == NULL so returning\n" << vprDEBUG_FLUSH;
      return;
   }
}

cfdVectors::~cfdVectors()
{ 
}

void cfdVectors::Update( void )
{
   if ( this->mapper && this->cursorType == CUBE )
   { 
      double bd[6];
      this->GetActiveMeshedVolume()->GetDataSet()->GetBounds( bd );
      vprDEBUG(vprDBG_ALL, 0) <<"d1:"<<bd[0]<<"d2:"<<bd[1]<<"d3:"<<bd[2]
          <<"d4:"<<bd[3]<<"d5:"<<bd[4]<<"d6:"<<bd[5]
          << std::endl << vprDEBUG_FLUSH;

      if ( this->origin[0] > (float)bd[0]-(this->box_size[1]-this->box_size[0])/2 
        && this->origin[0] < (float)bd[1]+(this->box_size[1]-this->box_size[0])/2 
        && this->origin[1] > (float)bd[2]-(this->box_size[3]-this->box_size[2])/2 
        && this->origin[1] < (float)bd[3]+(this->box_size[3]-this->box_size[2])/2
        && this->origin[2] > (float)bd[4]-(this->box_size[5]-this->box_size[4])/2
        && this->origin[2] < (float)bd[5]+(this->box_size[5]-this->box_size[4])/2 )
      {
         if ( this->box_size[0] != this->box_size[1] 
           && this->box_size[2] != this->box_size[3] 
           && this->box_size[4] != this->box_size[5] )
         {
            vprDEBUG(vprDBG_ALL, 0)
                <<"c1:"<<this->box_size[0]<<"c2:"<<this->box_size[1]
                <<"c3:"<<this->box_size[2]<<"c4:"<<this->box_size[3]
                <<"c5:"<<this->box_size[4]<<"c6:"<<this->box_size[5]
                << std::endl << vprDEBUG_FLUSH;

            this->filter->SetExtent( this->box_size );
            this->filter->ExtentClippingOn();
         }
         else
            this->filter->ExtentClippingOff();

         this->filter->Update();
      
         this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                           ->GetUserRange() );
         this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                           ->GetLookupTable() );
         this->mapper->Update();

         this->updateFlag = true;
      }
      else
      { 
         vprDEBUG(vprDBG_ALL, 0) <<"cfdVectors Error: cursor not in cube\n"
            << vprDEBUG_FLUSH;
         this->updateFlag = false;
      }
   }
   //make sure that there are planesData and that the cursorType is correct...
   else if ( this->mapper && this->cursorType == NONE )
   {
      // get every nth point from the dataSet data
      this->ptmask->SetInput( this->GetActiveMeshedVolume()
                       ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );
      this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
      this->ptmask->Update();

      // Not VTK Functions
      this->SetGlyphWithThreshold();
      this->SetGlyphAttributes();
      this->glyph->Update();
      //this->glyph->Print( cout );

      this->filter->ExtentClippingOff();
      this->filter->Update();
      
      // Good Test code to see if you are actually getting streamlines
/*      vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
      writer->SetInput( ( vtkPolyData * ) filter->GetOutput() );      
      writer->SetFileName( "teststreamers.vtk" );
      writer->Write();
*/     
      this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                        ->GetLookupTable() );
      this->mapper->Update();

      this->updateFlag = true;
   }
   else
   {
      this->updateFlag = false;
   }
}

