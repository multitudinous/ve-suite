/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdIsosurface.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdIsosurface.h"
#include "cfdDataSet.h"
#include "VE_SceneGraph/cfdGeode.h"

#include <vtkLookupTable.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkContourFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

cfdIsosurface::cfdIsosurface( int numsteps )
{
   this->totalId = numsteps;
   this->value = 0.0f;

#ifdef USE_OMP
   this->append = vtkAppendPolyData::New( );
   this->nData = this->GetActiveDataSet()->GetNoOfDataForProcs( );

   for ( int i=0; i<this->nData; i++ )
   {
      this->contour[i] = vtkContourFilter::New( );
      //this->contour[i]->UseScalarTreeOff( );
      this->contour[i]->UseScalarTreeOn();

      this->normals[i] = vtkPolyDataNormals::New( );
      this->normals[i]->SetInput( this->contour[i]->GetOutput( ) );

      this->append->AddInput( this->normals[i]->GetOutput( ) );
   }
#else
   this->contour = vtkContourFilter::New();
   this->contour->UseScalarTreeOn();
//   this->contour->UseScalarTreeOff();

   this->normals = vtkPolyDataNormals::New();
   this->normals->SetInput( this->contour->GetOutput() );
#endif

   this->filter = vtkGeometryFilter::New();
#ifdef USE_OMP
   this->filter->SetInput( (vtkDataSet *)this->append->GetOutput() );
#else
   this->filter->SetInput( (vtkDataSet *)this->normals->GetOutput() );
#endif

   this->mapper = vtkPolyDataMapper::New();
   this->mapper->SetInput( this->filter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();
}

cfdIsosurface::~cfdIsosurface()
{
   this->filter->Delete();
   this->mapper->Delete();
#ifdef USE_OMP
   for(int i = 0; this->nData; i++ )
   {
      this->contour[i]->Delete();
      this->normals[i]->Delete();
   }
   this->append->Delete();
#else
   this->contour->Delete();
   this->normals->Delete();
#endif
}

void cfdIsosurface::Update()
{
   vprDEBUG(vprDBG_ALL, 1) <<"\ncfdIsosurface::Update: FileName: "
      << this->GetActiveDataSet()->GetFileName() << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL, 1) << "this->requestedValue: "<< this->requestedValue
                           << std::endl << vprDEBUG_FLUSH;

   // convert the requested value percentage (0-100) to a scalar value
   this->value = convertPercentage( this->requestedValue );

   vprDEBUG(vprDBG_ALL, 1) << "this->value: "<< this->value
                           << std::endl << vprDEBUG_FLUSH;

#ifdef USE_OMP
   int imax = this->nData;
   int i;
# pragma omp parallel for private(i)
   for ( i=0; i<imax; i++ )
   {
      this->contour[i]->SetInput( this->GetActiveDataSet()->GetData(i) );
      this->contour[i]->SetValue( 0, this->value );
      this->normals[i]->Update();
   }
   this->append->Update( );
#else
   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdIsosurface: this->GetActiveMeshedVolume() = " 
      << this->GetActiveDataSet() << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL, 1) 
      << "cfdIsosurface: this->GetActiveMeshedVolume()->GetDataSet()=" 
      << this->GetActiveDataSet()->GetDataSet()
      << std::endl << vprDEBUG_FLUSH;

   this->contour->SetInput( this->GetActiveDataSet()->GetDataSet() );
   this->contour->SetValue( 0, this->value );
   //this->normals->Update();
#endif

   this->mapper->SetScalarRange( this->GetActiveDataSet()->GetUserRange() );
   this->mapper->SetLookupTable( this->GetActiveDataSet()->GetLookupTable() );
   this->mapper->Update();
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   geodes.push_back( new cfdGeode() );
   geodes.back()->TranslateTocfdGeode( temp );
   temp->Delete();
   this->updateFlag = true;
}

double cfdIsosurface::GetValue()
{
    return this->value;
}

double cfdIsosurface::convertPercentage( const int percentage )
{
   // set the step-size for isosurface based on the "pretty" range
   double minmax[2];
   this->GetActiveDataSet()->GetUserRange( minmax );

   vprDEBUG(vprDBG_ALL, 1) << "minmax = " << minmax[0] << "\t" << minmax[1]
                           << std::endl << vprDEBUG_FLUSH;

   double minmaxDiff = minmax[1] - minmax[0];
   double dx = ( minmaxDiff ) / (double)this->totalId;

   if( percentage == 999 )   // happens only with the blue menu
   {
      this->value += dx;

      // if way over the limit, reset close to bottom of range
      // (but true bottom will cause error)
      if ( this->value > minmax[1] + 0.5 * dx )
      {
         this->value = minmax[0] + minmaxDiff / 100.0;
      }

      // if just over the limit, reset close to end of range
      else if ( this->value > (minmax[1] - minmaxDiff / 100.0) )
      {
         this->value = minmax[1] - minmaxDiff / 100.0;
      }
   } 
   else
   {
      // The java app slider bar returns integers 0-100 representing percentile.  
      this->value = minmax[0] + minmaxDiff * percentage / 100.0;

      // if too low error will occur, so reset close to bottom of range
      if ( this->value < (minmax[0] + minmaxDiff / 100.0) )
      {
         this->value = minmax[0] + minmaxDiff / 100.0;
      }

      // if over the limit, reset close to end of range
      if ( this->value > (minmax[1] - minmaxDiff / 100.0) )
      {
         this->value = minmax[1] - minmaxDiff / 100.0;
      }
   }
   return this->value;
}

