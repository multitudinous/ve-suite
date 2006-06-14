/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdMomentums.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdEnum.h"    // needed for cursorType
#include "VE_Xplorer/cfdPlanes.h"
#include "VE_SceneGraph/cfdGeode.h"

#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkPointData.h>

#include "VE_Xplorer/cfdDebug.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdMomentums::cfdMomentums( const int xyz )
{
   this->xyz = xyz;
   this->warper = vtkWarpVector::New();
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
   if ( this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetNumberOfPlanes() == 0 )
   {
      vprDEBUG(vesDBG, 0) 
         << "cfdMomentums: planesData == NULL so returning"
         << std::endl << vprDEBUG_FLUSH;

      return;
   }

   //make sure that there are planesData and that the cursorType is correct...
   if ( this->mapper && this->cursorType == NONE )
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
      
      this->warper->SetInput( this->GetActiveDataSet()
                       ->GetPrecomputedSlices( this->xyz )->GetPlanesData() );
      this->warper->SetScaleFactor( this->warpedContourScale );
      this->warper->Update();//can this go???

      this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );

      this->mapper->SetScalarRange( this->GetActiveDataSet()->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()->GetLookupTable() );
      //this->mapper->Update();//can this go???

      vtkActor* temp = vtkActor::New();
      temp->SetMapper( this->mapper );
      //temp->GetProperty()->SetSpecularPower( 100.0f );
      //temp->GetProperty()->SetSpecular( 100.0f );
      //temp->GetProperty()->SetDiffuse( 100.0f );
      //temp->GetProperty()->SetAmbient( 100.0f );
      //geodes.push_back( new VE_SceneGraph::cfdGeode() );
      //geodes.back()->TranslateTocfdGeode( temp );
      //temp->Delete();
      //this->updateFlag = true;

      try
      {   
         VE_SceneGraph::cfdGeode* tempGeode = new VE_SceneGraph::cfdGeode();
         tempGeode->TranslateTocfdGeode( temp );
         geodes.push_back( tempGeode ); 
         this->updateFlag = true;
      }
      catch( std::bad_alloc )
      {
         mapper->Delete();
         mapper = vtkPolyDataMapper::New();

         vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdMomentums " 
                              << std::endl << vprDEBUG_FLUSH;
      }
      this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
      temp->Delete();
   }
   else
   {
      vprDEBUG(vesDBG, 0) 
         << "cfdMomentums::Update: !(mapper && cursorType == NONE)"
         << std::endl << vprDEBUG_FLUSH;

      this->updateFlag = false;
   }
}
