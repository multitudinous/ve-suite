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
#include <ves/xplorer/event/viz/cfdVector.h>
#ifdef USE_OMP
#include <vtkAppendFilter.h>
#include <omp.h>
#endif

#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/DataSet.h>

#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkCutter.h>
#include <vtkMultiGroupDataGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkActor.h>
#include <vtkMultiGroupPolyDataMapper.h>
#include <vtkProperty.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;

cfdVector::cfdVector()
{
#ifdef USE_OMP
   float b[6];
   float c[3];
   int maxPointsInPlanes = 0;
   this->nData = this->GetActiveDataSet()->GetNoOfDataForProcs();
   this->append = vtkAppendFilter::New();

   for ( int i=0; i<this->nData; i++ )
   {
      // get the center of data set.
      this->GetActiveDataSet()->GetData(i)->GetBounds( b );
      c[0] = b[1] - b[0];
      c[1] = b[3] - b[2];
      c[2] = b[5] - b[4];

      // set the plane
      this->plane[i] = vtkPlane::New();
      this->plane[i]->SetOrigin( c[0], c[1], c[2] );
      this->plane[i]->SetNormal( 1.0f, 0.0f, 0.0f );

      // set the cut function
      this->cutter[i] = vtkCutter::New();
      this->cutter[i]->SetInput( this->GetActiveDataSet()->GetData(i) );
      this->cutter[i]->SetCutFunction( this->plane[i] );

      if( this->cutter[i]->GetOutput()->GetPointData()->GetNumberOfTuples()
          > maxPointsInPlanes )
      {
         maxPointsInPlanes = this->cutter[i]->GetOutput()
                                         ->GetPointData()->GetNumberOfTuples();
      }
   }

   for ( int i=0; i<this->nData; i++ )
   {
      this->ptmask[i] = vtkMaskPoints::New();
      this->ptmask[i]->SetInput( this->cutter[i]->GetOutput() );
      this->ptmask[i]->SetOnRatio( this->GetVectorRatioFactor() );
      this->ptmask[i]->RandomModeOn();
      // Using glyph3D to insert arrow to the data sets
      this->glyph[i] = vtkGlyph3D::New(); 
      this->glyph[i]->SetInput( this->cutter[i]->GetOutput() );
      this->glyph[i]->SetSource( this->GetActiveDataSet()->GetArrow() );
      this->glyph[i]->SetScaleFactor( GetVectorScaleFactor() );
      this->glyph[i]->SetVectorModeToUseVector();
      this->glyph[i]->SetScaleModeToDataScalingOff();

      // append data
      this->append->AddInput( this->glyph[i]->GetOutput() );
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

#ifdef USE_OMP
   this->filter->SetInput( (vtkDataSet *)this->append->GetOutput() );
#else
   this->filter->SetInput( this->glyph->GetOutput() );
#endif

  //biv--do we need this? this->filter->ExtentClippingOn();
}

cfdVector::~cfdVector()
{
#ifdef USE_OMP
   for ( int i=0; i<this->nData; i++ )
   {
      this->plane[i]->Delete();
      this->cutter[i]->Delete();
      this->glyph[i]->Delete();
      this->ptmask[i]->Delete();
   }
   this->append->Delete();
   this->append = NULL;
#else
   this->plane->Delete();
   this->plane = NULL;

   this->cutter->Delete();
   this->cutter = NULL;
#endif
}

void cfdVector::Update( void )
{
   if ( this->cursorType == ARROW )
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
         this->ptmask[i]->Update();
         this->glyph[i]->Update();
      }
#else
      vprDEBUG(vesDBG, 1) << "Vector Cutting Plane Update"
                              << std::endl << vprDEBUG_FLUSH;
      this->plane->SetOrigin( this->origin );
      vprDEBUG(vesDBG, 1) << "origin: " << this->origin[0] << " : " 
                              << this->origin[1] << " : " << this->origin[2] 
                              << std::endl << vprDEBUG_FLUSH;
      this->plane->SetNormal( this->normal );
      this->cutter->SetInput( this->GetActiveDataSet()->GetDataSet() );
      int numPointsInPlanes = 0;
      numPointsInPlanes = this->cutter->GetOutput()->GetPointData()->GetNumberOfTuples();
      vprDEBUG(vesDBG, 3) << "|   Number of points in cutting plane : " << numPointsInPlanes
                              << std::endl << vprDEBUG_FLUSH;

      // get every nth point from the dataSet data
      this->ptmask->SetInput( (vtkDataSet *)this->cutter->GetOutput() );
      this->ptmask->SetOnRatio( this->GetVectorRatioFactor() );

      // Using glyph3D to insert arrow to the data sets
      this->glyph->SetInput( (vtkDataSet *)this->ptmask->GetOutput() );
      this->glyph->SetScaleFactor( GetVectorScaleFactor() );
      this->glyph->SetScaleModeToDataScalingOff();
      this->cutter->Update();
      this->ptmask->Update();
      this->glyph->Update();
#endif
      this->filter->Update();

      this->mapper->SetScalarRange( this->GetActiveDataSet()
                                        ->GetUserRange() );
      this->mapper->SetLookupTable( this->GetActiveDataSet()
                                        ->GetLookupTable() );
      this->mapper->Update();

   }
   else if ( this->cursorType == CUBE )
   {
      vprDEBUG(vesDBG, 0) << "cfdVector not implemented for cube cursor"
                              << std::endl << vprDEBUG_FLUSH;
   }
   
   vtkActor* temp = vtkActor::New();
   temp->SetMapper( this->mapper );
   temp->GetProperty()->SetSpecularPower( 20.0f );
   
   try
   {
		osg::ref_ptr< ves::xplorer::scenegraph::Geode > tempGeode = new ves::xplorer::scenegraph::Geode();
      tempGeode->TranslateToGeode( temp );
      geodes.push_back( tempGeode.get() ); 
      this->updateFlag = true;
   }
   catch( std::bad_alloc )
   {
      mapper->Delete();
      mapper = vtkMultiGroupPolyDataMapper::New();
      vprDEBUG(vesDBG,0) << "|\tMemory allocation failure : cfdVector " 
                           << std::endl << vprDEBUG_FLUSH;
   }
  temp->Delete();
}

