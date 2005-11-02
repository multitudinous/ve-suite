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
 * File:          $RCSfile: cfdVectorBase.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdVectorBase.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"

#include <cmath>

#include "VE_Xplorer/cfdDebug.h"

#include <vtkPolyData.h>
#include <vtkGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>

using namespace VE_Xplorer;

// this class requires that the dataset has a vector field.
cfdVectorBase::cfdVectorBase()
{
   vprDEBUG(vesDBG,2) << "cfdVectorBase constructor"
                          << std::endl  << vprDEBUG_FLUSH;

   this->ptmask = vtkMaskPoints::New();
   this->ptmask->RandomModeOn();

   // Using glyph3D to insert arrow to the data sets
   this->glyph = vtkGlyph3D::New(); 

   tfilter = vtkThresholdPoints::New();
   this->filter = vtkGeometryFilter::New();
   this->filter->SetInput( this->glyph->GetOutput() );
//   filter->GetOutput()->ReleaseDataFlagOn();

   this->tris = vtkTriangleFilter::New();
   this->strip = vtkStripper::New();

   this->mapper = vtkPolyDataMapper::New();
   this->mapper->SetInput( this->filter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();
   mapper->ImmediateModeRenderingOn();   

   _vectorScale = 1.0;
   _vectorThreshHoldMinPercentage = 0;
   _vectorThreshHoldMaxPercentage = 100;
   _vectorThreshHoldValues[ 0 ] = 0.0;
   _vectorThreshHoldValues[ 1 ] = 100.0;
   _scaleByVector = 0;
   _vectorRatioFactor = 1;
}


cfdVectorBase::~cfdVectorBase()
{
   vprDEBUG(vesDBG,2) << "cfdVectorBase destructor"
                          << std::endl  << vprDEBUG_FLUSH;

   tfilter->Delete();
   tfilter = 0;

   this->ptmask->Delete();
   this->ptmask = 0;
   
   this->glyph->Delete();
   this->glyph = 0;
   
   this->filter->Delete();
   this->filter = 0;

   this->tris->Delete();
   this->tris = 0;
   
   this->strip->Delete();
   this->strip = 0;
   
   
   this->mapper->Delete();
   this->mapper = 0;
}

bool cfdVectorBase::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_THRESHOLD )
   { 
      vprDEBUG(vesDBG,0) << " cfdVectorBase::CheckCommandId : CHANGE_VECTOR_THRESHOLD" 
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;

      SetThreshHoldPercentages( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ),
                                               (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      UpdateThreshHoldValues();

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_MASK_RATIO )
   { 
      vprDEBUG(vesDBG,0) << " CHANGE_VECTOR_MASK_RATIO" 
         << ", value = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      SetVectorRatioFactor( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_VECTOR_SCALE )
   {
      vprDEBUG(vesDBG,0) << " CHANGE_VECTOR_SCALE" 
         << ", value = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      SetVectorScale( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == SCALE_BY_VECTOR_MAGNITUDE )
   { 
      vprDEBUG(vesDBG,0)
         << "SCALE_BY_VECTOR_MAGNITUDE = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      SetScaleByVectorFlag( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      return true;
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STEADYSTATE_DATASET )
   { 
      SetThreshHoldPercentages( 0, 100 );
      SetVectorRatioFactor( 1 );
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
   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}
//////////////////////////////////////////////
void cfdVectorBase::SetVectorScale( float x )
{
   _vectorScale = x;
}
/////////////////////////////////////
float cfdVectorBase::GetVectorScale()
{
   return _vectorScale;
}
///////////////////////////////////////////
void cfdVectorBase::SetGlyphWithThreshold()
{
   vprDEBUG(vesDBG, 1) << "vectorThreshHoldValues : " 
      << _vectorThreshHoldValues[ 0 ] << " : " 
      << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;

   double currentScalarRange[ 2 ];
   this->GetActiveDataSet()->GetRange( currentScalarRange );

   if ( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] && 
        _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdBetween"
         << std::endl << vprDEBUG_FLUSH;
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdBetween( _vectorThreshHoldValues[ 0 ],
                                 _vectorThreshHoldValues[ 1 ] );
      //tfilter->Update();
      this->tris->SetInput(this->tfilter->GetOutput());
      this->strip->SetInput(this->tris->GetOutput());
      this->glyph->SetInput(this->strip->GetOutput());
      //this->glyph->SetInput( tfilter->GetOutput());
   }
   else if ( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdByUpper"
         << std::endl << vprDEBUG_FLUSH;
      //vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByUpper( _vectorThreshHoldValues[ 0 ] );
      //tfilter->Update();
      this->tris->SetInput(this->tfilter->GetOutput());
      this->strip->SetInput(this->tris->GetOutput());
      this->glyph->SetInput( this->strip->GetOutput());
      //this->glyph->SetInput( tfilter->GetOutput());
   }
   else if ( _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdByLower"
         << std::endl << vprDEBUG_FLUSH;
      //vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByLower( _vectorThreshHoldValues[ 1 ] );
      //tfilter->Update();
      this->tris->SetInput(this->tfilter->GetOutput());
      this->strip->SetInput(this->tris->GetOutput());
      this->glyph->SetInput(this->strip->GetOutput());
      //this->glyph->SetInput( tfilter->GetOutput());
   }
   else
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: NO Threshold"
         << std::endl << vprDEBUG_FLUSH;
      this->glyph->SetInput( this->ptmask->GetOutput() );
   }
}
///////////////////////////////////////////
/*void cfdVectorBase::SetGlyphWithThreshold()
{
   vprDEBUG(vesDBG, 1) << "vectorThreshHoldValues : " 
      << _vectorThreshHoldValues[ 0 ] << " : " 
      << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;

   double currentScalarRange[ 2 ];
   this->GetActiveDataSet()->GetRange( currentScalarRange );

   if ( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] && 
        _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdBetween"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdBetween( _vectorThreshHoldValues[ 0 ],
                                 _vectorThreshHoldValues[ 1 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else if ( _vectorThreshHoldValues[ 0 ] > currentScalarRange[ 0 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdByUpper"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByUpper( _vectorThreshHoldValues[ 0 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else if ( _vectorThreshHoldValues[ 1 ] < currentScalarRange[ 1 ] )
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: ThresholdByLower"
         << std::endl << vprDEBUG_FLUSH;
      vtkThresholdPoints * tfilter = vtkThresholdPoints::New();
      tfilter->SetInput( this->ptmask->GetOutput() );
      tfilter->ThresholdByLower( _vectorThreshHoldValues[ 1 ] );
      tfilter->Update();
      this->glyph->SetInput( tfilter->GetOutput());
      tfilter->Delete(); 
   }
   else
   {
      vprDEBUG(vesDBG, 1) <<"cfdVectorBase: NO Threshold"
         << std::endl << vprDEBUG_FLUSH;
      this->glyph->SetInput( this->ptmask->GetOutput() );
   }
}*/

void cfdVectorBase::SetGlyphAttributes()
{
   this->glyph->SetSource( this->GetActiveDataSet()->GetArrow() );
   this->glyph->SetVectorModeToUseVector();
   //this->glyph->DebugOn();

   this->glyph->SetScaleFactor( GetVectorScaleFactor() );
   if ( _scaleByVector == 0 )
   {  
      this->glyph->SetScaleModeToDataScalingOff();
   }
   else
   {
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

   vprDEBUG(vesDBG, 1) << " scaleFactor = " << scaleFactor 
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
      vprDEBUG(vesDBG, 0) << "updating cutter to compute numPointsInPlane"
                              << std::endl  << vprDEBUG_FLUSH;
      this->cutter->Update();
      int numPointsInPlane = this->cutter->GetOutput()->GetNumberOfPoints();
      ratioFactor = numPointsInPlane/1000;
   }

   if ( ratioFactor < 1 ) 
   {
      ratioFactor = 1;
   }

   vprDEBUG(vesDBG, 1) << "ratioFactor = " << ratioFactor 
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

   vprDEBUG(vesDBG, 1) << "numPlanes = " << numPlanes
      << ", numPointsInPlanes = " << numPointsInPlanes 
      << ", ratioFactor = " << ratioFactor 
      << std::endl << vprDEBUG_FLUSH;

   return ratioFactor;
}
*/


void cfdVectorBase::UpdateThreshHoldValues()
{
   //double currentScalarRange[ 2 ];
   cfdDataSet* temp = GetActiveDataSet();

   if ( temp != NULL )
   {
      //temp->GetRange( currentScalarRange );
      double* currentScalarRange = temp->GetRange();
      vprDEBUG(vesDBG,1) << " currentScalarRange = " 
         << currentScalarRange[ 0 ] << " : " <<  currentScalarRange[ 1 ] 
         << std::endl << vprDEBUG_FLUSH;

      _vectorThreshHoldValues[ 0 ] = currentScalarRange[0] +
                    (double)_vectorThreshHoldMinPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      _vectorThreshHoldValues[ 1 ] = currentScalarRange[0] +
                    (double)_vectorThreshHoldMaxPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      vprDEBUG(vesDBG, 1) << " Threshold Values = "
         << _vectorThreshHoldValues[ 0 ] << " : "
         << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
   }
}
void cfdVectorBase::SetThreshHoldPercentages( int minThread, int maxThread )
{
   _vectorThreshHoldMinPercentage = minThread;
   _vectorThreshHoldMaxPercentage = maxThread;
}
void cfdVectorBase::SetThreshHoldValues( double* input )
{
   _vectorThreshHoldValues[ 0 ] = input[ 0 ];
   _vectorThreshHoldValues[ 1 ] = input[ 1 ];

   vprDEBUG(vesDBG, 1) << " Threshold Values = "
      << _vectorThreshHoldValues[ 0 ] << " : "
      << _vectorThreshHoldValues[ 1 ] << std::endl << vprDEBUG_FLUSH;
}
double* cfdVectorBase::GetThreshHoldValues( void )
{
   return ( _vectorThreshHoldValues );
}
void cfdVectorBase::SetVectorRatioFactor( int x )
{
   _vectorRatioFactor = x;
}

int cfdVectorBase::GetVectorRatioFactor()
{
   return _vectorRatioFactor;
}
void cfdVectorBase::SetScaleByVectorFlag( int input )
{
   _scaleByVector = input;
}
int cfdVectorBase::GetScaleByVectorFlag( void )
{
   return _scaleByVector;
}

