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
 * File:          $RCSfile: cfdVectorBase.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdVectorBase.h"
#include "cfdDataSet.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"

#include <cmath>

#include <vpr/Util/Debug.h>

#include <vtkPolyData.h>
#include <vtkGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

// this class requires that the dataset has a vector field.
cfdVectorBase::cfdVectorBase()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdVectorBase constructor"
                          << std::endl  << vprDEBUG_FLUSH;

   this->ptmask = vtkMaskPoints::New();
   this->ptmask->RandomModeOn();

   // Using glyph3D to insert arrow to the data sets
   this->glyph = vtkGlyph3D::New(); 

   this->filter = vtkGeometryFilter::New();
   this->filter->SetInput( this->glyph->GetOutput() );

   this->mapper = vtkPolyDataMapper::New();
   this->mapper->SetInput( this->filter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );
}


cfdVectorBase::~cfdVectorBase()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdVectorBase destructor"
                          << std::endl  << vprDEBUG_FLUSH;

   this->ptmask->Delete();
   this->ptmask = NULL;
   
   this->glyph->Delete();
   this->glyph = NULL;
   
   this->filter->Delete();
   this->filter = NULL;
   
   this->mapper->Delete();
   this->mapper = NULL;

   this->actor->Delete();
   this->actor = NULL;
}

bool cfdVectorBase::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_THRESHOLD )
   { 
      vprDEBUG(vprDBG_ALL,0) << " cfdVectorBase::CheckCommandId : CHANGE_VECTOR_THRESHOLD" 
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetThreshHoldPercentages( commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ),
                                               commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      UpdateThreshHoldValues();

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_MASK_RATIO )
   { 
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR_MASK_RATIO" 
         << ", value = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetVectorRatioFactor( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_SCALE )
   {
      vprDEBUG(vprDBG_ALL,0) << " CHANGE_VECTOR_SCALE" 
         << ", value = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdObjects::SetVectorScale( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == SCALE_BY_VECTOR_MAGNITUDE )
   { 
      vprDEBUG(vprDBG_ALL,0)
         << "SCALE_BY_VECTOR_MAGNITUDE = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      cfdVectorBase::SetScaleByVectorFlag( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEADYSTATE_DATASET )
   { 
      cfdVectorBase::SetThreshHoldPercentages( 0, 100 );
      cfdVectorBase::SetVectorRatioFactor( 1 );
      //this->UpdateThreshHoldValues();
   }

   // when scalar is changed reset vector thresholding to none...
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR  )
   {
      //UpdateThreshHoldValues();
   }
   return flag;
}

void cfdVectorBase::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}

void cfdVectorBase::SetGlyphWithThreshold()
{
   vprDEBUG(vprDBG_ALL, 1) << "vectorThreshHoldValues : " 
      << vectorThreshHoldValues[ 0 ] << " : " 
      << vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;

   double currentScalarRange[ 2 ];
   this->GetActiveDataSet()->GetRange( currentScalarRange );

   if ( vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] && 
        vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vprDBG_ALL, 1) <<"cfdVectorBase: ThresholdBetween"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdBetween( vectorThreshHoldValues[ 0 ],
                                 vectorThreshHoldValues[ 1 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else if ( vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )
   {
      vprDEBUG(vprDBG_ALL, 1) <<"cfdVectorBase: ThresholdByUpper"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByUpper( vectorThreshHoldValues[ 0 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else if ( vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vprDBG_ALL, 1) <<"cfdVectorBase: ThresholdByLower"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByLower( vectorThreshHoldValues[ 1 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 1) <<"cfdVectorBase: NO Threshold"
         << std::endl << vprDEBUG_FLUSH;
      this->glyph->SetInput( this->ptmask->GetOutput() );
   }
}

void cfdVectorBase::SetGlyphAttributes()
{
   this->glyph->SetSource( this->GetActiveDataSet()->GetArrow() );
   this->glyph->SetVectorModeToUseVector();
   //this->glyph->DebugOn();

   if ( scaleByVector == 0 )
   {  
      this->glyph->SetScaleFactor( GetVectorScaleFactor() );
      this->glyph->SetScaleModeToDataScalingOff();
   }
   else
   {
      this->glyph->SetScaleFactor( GetVectorScaleFactor() );
      this->glyph->SetScaleModeToScaleByVector();
      this->glyph->SetColorModeToColorByScalar();
      this->glyph->ClampingOn();
      this->glyph->SetRange( this->GetActiveDataSet()->GetVectorMagRange() );
   }
}

float cfdVectorBase::GetVectorScaleFactor()
{
   // this->GetVectorScale() is obtained from gui, -100 < vectorScale < 100
   // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
   // convert range to -2.5 < x < 2.5, and compute the exponent...
   float range = 2.5;
   float scaleFactor = exp( this->GetVectorScale() / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetMeanCellLength();

   vprDEBUG(vprDBG_ALL, 1) << " scaleFactor = " << scaleFactor 
                           << std::endl << vprDEBUG_FLUSH;

   return scaleFactor;
}

/*
int cfdVectorBase::GetVectorRatioFactor()
{
   int ratioFactor = this->ratio;
   if ( ! ratioFactor )
   {
      // must update to allow numPointsInPlane computation to work
      vprDEBUG(vprDBG_ALL, 0) << "updating cutter to compute numPointsInPlane"
                              << std::endl  << vprDEBUG_FLUSH;
      this->cutter->Update();
      int numPointsInPlane = this->cutter->GetOutput()->GetNumberOfPoints();
      ratioFactor = numPointsInPlane/1000;
   }

   if ( ratioFactor < 1 ) 
   {
      ratioFactor = 1;
   }

   vprDEBUG(vprDBG_ALL, 1) << "ratioFactor = " << ratioFactor 
                           << std::endl << vprDEBUG_FLUSH;

   return ratioFactor;
}
*/
/*
int cfdVectors::GetVectorRatioFactor()
{
   int ratioFactor = this->ratio;
   int numPointsInPlanes = 0;
   int numPlanes = 0;

   if ( ! ratioFactor )
   {
      numPointsInPlanes = this->GetActiveMeshedVolume()->GetPrecomputedSlices( xyz )
                                  ->GetPlanesData()->GetNumberOfPoints();
      numPlanes = this->GetActiveMeshedVolume()->GetPrecomputedSlices( xyz )
                          ->GetNumberOfPlanes();
      ratioFactor = numPointsInPlanes/(1000*numPlanes);
   }

   if ( ratioFactor < 1 ) 
      ratioFactor = 1;

   vprDEBUG(vprDBG_ALL, 1) << "numPlanes = " << numPlanes
      << ", numPointsInPlanes = " << numPointsInPlanes 
      << ", ratioFactor = " << ratioFactor 
      << std::endl << vprDEBUG_FLUSH;

   return ratioFactor;
}
*/


void cfdVectorBase::UpdateThreshHoldValues()
{
   //double currentScalarRange[ 2 ];
std::cout << " UpdateThreshHoldValues " <<  std::endl;
   cfdDataSet* temp = GetActiveDataSet();
std::cout << " UpdateThreshHoldValues " <<  std::endl;
   if ( temp != NULL )
   {
      //temp->GetRange( currentScalarRange );
      double* currentScalarRange = temp->GetRange();
      vprDEBUG(vprDBG_ALL,1) << " currentScalarRange = " 
         << currentScalarRange[ 0 ] << " : " <<  currentScalarRange[ 1 ] 
         << std::endl << vprDEBUG_FLUSH;

      vectorThreshHoldValues[ 0 ] = currentScalarRange[0] +
                    (double)vectorThreshHoldMinPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      vectorThreshHoldValues[ 1 ] = currentScalarRange[0] +
                    (double)vectorThreshHoldMaxPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      vprDEBUG(vprDBG_ALL, 1) << " Threshold Values = "
         << vectorThreshHoldValues[ 0 ] << " : "
         << vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
   }
}


/////////////////// STATIC member functions follow ///////////////////

// initial definition of the static variable
int cfdVectorBase::vectorThreshHoldMinPercentage = 0;
int cfdVectorBase::vectorThreshHoldMaxPercentage = 100;

void cfdVectorBase::SetThreshHoldPercentages( int min, int max )
{
   vectorThreshHoldMinPercentage = min;
   vectorThreshHoldMaxPercentage = max;
}

double cfdVectorBase::vectorThreshHoldValues[ 2 ] = { 0.0, 100.0 };

void cfdVectorBase::SetThreshHoldValues( double* input )
{
   vectorThreshHoldValues[ 0 ] = input[ 0 ];
   vectorThreshHoldValues[ 1 ] = input[ 1 ];

   vprDEBUG(vprDBG_ALL, 1) << " Threshold Values = "
      << vectorThreshHoldValues[ 0 ] << " : "
      << vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
}

double* cfdVectorBase::GetThreshHoldValues( void )
{
   return ( vectorThreshHoldValues );
}


int cfdVectorBase::vectorRatioFactor = 1;

void cfdVectorBase::SetVectorRatioFactor( int x )
{
   vectorRatioFactor = x;
}

int cfdVectorBase::GetVectorRatioFactor()
{
   return vectorRatioFactor;
}

int cfdVectorBase::scaleByVector = 0;

void cfdVectorBase::SetScaleByVectorFlag( int input )
{
   scaleByVector = input;
}

int cfdVectorBase::GetScaleByVectorFlag( void )
{
   return scaleByVector;
}

