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
#include "VE_Xplorer/XplorerHandlers/cfdVectors.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdPlanes.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"

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
#include <vtkPointData.h>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_Xplorer;

cfdVectors::cfdVectors( const int xyz )
{
   this->xyz = xyz;
}

cfdVectors::~cfdVectors()
{ 
}

void cfdVectors::Update( void )
{
std::cout << " vectors update " << this->xyz << " " << this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetNumberOfPlanes() << std::endl;
   if ( this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetNumberOfPlanes() == 0 )
   {
      vprDEBUG(vesDBG, 0) 
         << "cfdVectors, planesData has 0 planes so returning\n" << vprDEBUG_FLUSH;
      return;
   }

   if ( this->mapper && this->cursorType == CUBE )
   { 
      
     // this->planes = new cfdPlanes();
     // this->planes->SetAllPlanesSelected();
     // this->planes->ConcatenateSelectedPlanes();

      double bd[6];
      this->GetActiveDataSet()->GetDataSet()->GetBounds( bd );
      vprDEBUG(vesDBG, 0) <<"d1:"<<bd[0]<<"d2:"<<bd[1]<<"d3:"<<bd[2]
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
            vprDEBUG(vesDBG, 0)
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
      
         this->mapper->SetScalarRange( this->GetActiveDataSet()
                                           ->GetUserRange() );
         this->mapper->SetLookupTable( this->GetActiveDataSet()
                                           ->GetLookupTable() );
         this->mapper->Update();

         this->updateFlag = true;
      }
      else
      { 
         vprDEBUG(vesDBG, 0) <<"cfdVectors Error: cursor not in cube\n"
            << vprDEBUG_FLUSH;
         this->updateFlag = false;
      }
   }
   //make sure that there are planesData and that the cursorType is correct...
   else if ( this->mapper && this->cursorType == NONE )
   {
      this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->SetAllPlanesSelected();
      this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->ConcatenateSelectedPlanes();

      std::string vectorName = this->GetActiveDataSet()->
                                 GetVectorName( this->GetActiveDataSet()->GetActiveVector() );
      this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )
            ->GetPlanesData()->GetPointData()->SetActiveVectors( vectorName.c_str() );

      std::string scalarName = this->GetActiveDataSet()->
                                 GetScalarName( this->GetActiveDataSet()->GetActiveScalar() );
      this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )
               ->GetPlanesData()->GetPointData()->SetActiveScalars( scalarName.c_str() );

      // get every nth point from the dataSet data
      this->ptmask->SetInput( this->GetActiveDataSet()
                       ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );
      this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );
      this->ptmask->Update();

      // Not VTK Functions
      this->SetGlyphWithThreshold();
      this->SetGlyphAttributes();
      //this->glyph->Update();  
      //this->glyph->Print( cout );

      this->filter->ExtentClippingOff();
      this->filter->Update();
      
      // Good Test code to see if you are actually getting streamlines
/*      
      vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
      writer->SetInput( ( vtkPolyData * ) filter->GetOutput() );      
      writer->SetFileName( "teststreamers.vtk" );
      writer->Write();
*/     
      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
     // this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();

      //this->mapper->Update();  //sgent
   }
   else
   {
      this->updateFlag = false;
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
      geodes.push_back( tempGeode ); 
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkPolyDataMapper::New();
      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdVectors " 
                           << std::endl << vprDEBUG_FLUSH;
   }
   this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
   temp->Delete();
}

