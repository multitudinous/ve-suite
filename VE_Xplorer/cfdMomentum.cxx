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
 * File:          $RCSfile: cfdMomentum.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdMomentum.h"
#include "cfdDataSet.h"
#include "cfdEnum.h"    // needed for cursorType

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkWarpVector.h>
#include <vtkPolyDataMapper.h>

#include <vpr/Util/Debug.h>

cfdMomentum::cfdMomentum( )
{
#ifdef USE_OMP  
   float b[6];
   float c[3];
   this->nData = this->GetActiveMeshedVolume()->GetNoOfDataForProcs( );

   this->append = vtkAppendPolyData::New( );

   for ( int i=0; i<this->nData; i++ )
   {
      // get the center of data set.
      this->GetActiveMeshedVolume()->GetDataSet(i)->GetBounds( b );
      c[0] = b[1] - b[0];
      c[1] = b[3] - b[2];
      c[2] = b[5] - b[4];

      // set the plane
      this->plane[i] = vtkPlane::New( );
      this->plane[i]->SetOrigin( c[0], c[1], c[2] );
      this->plane[i]->SetNormal( 1.0f, 0.0f, 0.0f );

      // set the cut function
      this->cutter[i] = vtkCutter::New( );
      this->cutter[i]->SetInput( this->GetActiveMeshedVolume()->GetDataSet(i) );
      this->cutter[i]->SetCutFunction( this->plane[i] );

      // append data
      this->append->AddInput( this->cutter[i]->GetOutput( ) );
   }
#else

   // set the plane
   this->plane = vtkPlane::New();
   this->plane->SetOrigin( 0.0f, 0.0f, 0.0f );
   this->plane->SetNormal( 1.0f, 0.0f, 0.0f );

   // set the cut function
   this->cutter = vtkCutter::New();
   this->cutter->SetCutFunction( this->plane );
#endif

   this->warper = vtkWarpVector::New();
#ifdef USE_OMP
   this->warper->SetInput( (vtkPointSet *)this->append->GetOutput() );
#else
   this->warper->SetInput( (vtkPointSet *)this->cutter->GetOutput() );
#endif

   this->warper->SetScaleFactor( this->GetActiveMeshedVolume()->GetStepLength() );

}


cfdMomentum::~cfdMomentum()
{
#ifdef USE_OMP
   for( int i = 0; i < this->nData; i++ )
   {
      this->plane[i]->Delete();
      this->cutter[i]->Delete();
   }
   this->append->Delete();
#else
   this->plane->Delete();
   this->cutter->Delete();
#endif

   this->warper->Delete();
}


void cfdMomentum::Update( void )
{    
   if ( this->cursorType == ARROW )
   {
/*
      // get the boundary of data set
      float bd[6];
      this->GetActiveMeshedVolume()->GetDataSet()->GetBounds( bd );

      if ( this->origin[0] > bd[0] && this->origin[0] < bd[1] &&
           this->origin[1] > bd[2] && this->origin[1] < bd[3] &&
           this->origin[2] > bd[4] && this->origin[2] < bd[5] )
*/
      {  

#ifdef USE_OMP
         int i;
         int imax = this->nData;

# pragma omp parallel for private(i)
         for ( i=0; i<imax; i++ )
         {
            this->plane[i]->SetOrigin( this->origin );
            this->plane[i]->SetNormal( this->normal );
            this->cutter[i]->Update();
         }
         this->append->Update( );
#else
         this->cutter->SetInput( this->GetActiveMeshedVolume()->GetDataSet() );
         this->plane->SetOrigin( this->origin );
         this->plane->SetNormal( this->normal );
         this->cutter->Update();
#endif
         this->SetMapperInput( (vtkPolyData*)this->warper->GetOutput() );
         this->mapper->SetScalarRange( this->GetActiveMeshedVolume()
                                           ->GetUserRange() );
         this->mapper->SetLookupTable( this->GetActiveMeshedVolume()
                                           ->GetLookupTable() );
         this->mapper->Update();

         this->updateFlag = true;
      }
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) << "cfdMomentum requires cursorType == ARROW"
                              << std::endl << vprDEBUG_FLUSH;
      this->updateFlag = false;
   }
}

