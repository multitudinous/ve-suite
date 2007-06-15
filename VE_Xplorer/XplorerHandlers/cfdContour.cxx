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
#include "VE_Xplorer/XplorerHandlers/cfdContour.h"

#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"    // needed for cursorType

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkCutter.h>
#include <vtkGeometryFilter.h>   // for inherited contourBase member filter
#include <vtkPolyDataMapper.h>   // for inherited contourBase member mapper
#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkMultiGroupPolyDataMapper.h>
#ifdef USE_OMP
#include <vtkAppendPolyData.h>
#endif

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdContour::cfdContour()
:cfdContourBase()
{
   vprDEBUG(vesDBG,2) << "cfdContour constructor"
                          << std::endl << vprDEBUG_FLUSH;
#ifdef USE_OMP 
   float b[6];
   float c[3];

   this->append = vtkAppendPolyData::New();
   this->nData = this->GetActiveMeshedVolume()->GetNoOfDataForProcs();

   for ( int i = 0; i < this->nData; i++ )
   {
      this->GetActiveMeshedVolume()->GetData(i)->GetBounds( b );
      c[0] = b[1] - b[0];
      c[1] = b[3] - b[2];
      c[2] = b[5] - b[4];

      // set the plane
      this->plane[i] = vtkPlane::New();
      this->plane[i]->SetOrigin( c[0], c[1], c[2] );
      this->plane[i]->SetNormal( 1.0f, 0.0f, 0.0f );

      // set the cut function
      this->cutter[i] = vtkCutter::New();
      this->cutter[i]->SetInput( this->GetActiveMeshedVolume()->GetData(i) );
      this->cutter[i]->SetCutFunction( this->plane[i] );

      // append data
      this->append->AddInput( this->cutter[i]->GetOutput() );
   }
#else

   // set the contour visualization pipeline
   this->plane = vtkPlane::New();
   this->plane->SetOrigin( 0.0f, 0.0f, 0.0f );
   this->plane->SetNormal( 1.0f, 0.0f, 0.0f );

   // set the cut function
   this->cutter = vtkCutter::New();
   this->cutter->SetCutFunction( this->plane );

#endif

   //this->filter->ExtentClippingOn();
}

cfdContour::~cfdContour()
{
   vprDEBUG(vesDBG,2) << "cfdContour destructor"
                          << std::endl << vprDEBUG_FLUSH;
#ifdef USE_OMP
   for ( int i=0; i<this->nData; i++ )
   {
      this->plane[i]->Delete();
      this->cutter[i]->Delete();
   }
   this->append->Delete();
#else
   this->plane->Delete();
   this->cutter->Delete();
#endif
}

void cfdContour::Update( void )
{
   if ( this->cursorType == CUBE )
   {
/*
      float bd[6];      // Boundary of the whole data sets.
      this->GetActiveMeshedVolume()->GetDataSet()->GetBounds( bd );
      if (  this->center[0] > bd[0]-(this->box_size[1]-this->box_size[0])/2 
         && this->center[0] < bd[1]+(this->box_size[1]-this->box_size[0])/2 
         && this->center[1] > bd[2]-(this->box_size[3]-this->box_size[2])/2 
         && this->center[1] < bd[3]+(this->box_size[3]-this->box_size[2])/2
         && this->center[2] > bd[4]-(this->box_size[5]-this->box_size[4])/2
         && this->center[2] < bd[5]+(this->box_size[5]-this->box_size[4])/2 )
*/
      {  
#ifndef USE_OMP
         this->plane->SetOrigin( this->center );
         this->plane->SetNormal( this->normal );
         this->cutter->Update();
         this->SetMapperInput( this->cutter->GetOutput() );
         this->updateFlag = true;
#endif
      }
   }
   else if ( this->cursorType == ARROW )
   {
      vprDEBUG(vesDBG, 0) << "contour cutting plane origin: "
         << origin[0] << " : " << origin[1] << " : " << origin[2]
         << std::endl << vprDEBUG_FLUSH;
#ifdef USE_OMP
      int num_data = this->nData;
      int i;

# pragma omp parallel for private(i)
      for ( i=0; i<num_data; i++ )
      {
         this->plane[i]->SetOrigin( this->origin );
         this->plane[i]->SetNormal( this->normal );
         this->cutter[i]->Update();
      }
      this->SetMapperInput( this->append->GetOutput() );

#else

      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      //this->cutter->SetInput( this->GetActiveMeshedVolume()->getProbe(cursor->getBox())->GetOutput() ); //temp change
      this->plane->SetOrigin( this->origin );
      this->plane->SetNormal( this->normal );
      this->cutter->Update();
      this->SetMapperInput( this->cutter->GetOutput() );

#endif

      this->mapper->SetScalarRange( 
                               this->GetActiveDataSet()->GetUserRange() );

      this->mapper->SetLookupTable( 
                             this->GetActiveDataSet()->GetLookupTable() );

      this->updateFlag = true;
   }
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   //geodes.push_back( new VE_SceneGraph::Geode() );
   //geodes.back()->TranslateToGeode( temp );
   //temp->Delete();

   try
   {
		osg::ref_ptr< VE_SceneGraph::Geode > tempGeode = new VE_SceneGraph::Geode();
      tempGeode->TranslateToGeode( temp );
      geodes.push_back( tempGeode.get() );
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkMultiGroupPolyDataMapper::New();

      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdContour" 
                           << std::endl << vprDEBUG_FLUSH;
   }
   //this->GetActiveDataSet()->GetPrecomputedSlices( this->xyz )->GetPlanesData()->Delete();
   temp->Delete();
}

