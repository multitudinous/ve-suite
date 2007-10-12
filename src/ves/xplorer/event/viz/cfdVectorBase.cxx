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
#include <ves/xplorer/event/viz/cfdVectorBase.h>
#include <ves/xplorer/cfdDataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/cfdCommandArray.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <cmath>

#include <ves/xplorer/cfdDebug.h>

#include <vtkPolyData.h>
#include <vtkMultiGroupDataGeometryFilter.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vtkActor.h>
#include <vtkMultiGroupPolyDataMapper.h>
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
   this->filter = vtkMultiGroupDataGeometryFilter::New();
   this->filter->SetInputConnection( this->glyph->GetOutputPort() );
//   filter->GetOutput()->ReleaseDataFlagOn();

   this->tris = vtkTriangleFilter::New();
   this->strip = vtkStripper::New();

   this->mapper = vtkMultiGroupPolyDataMapper::New();
   this->mapper->SetInputConnection( this->filter->GetOutputPort() );
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
   //vprDEBUG(vesDBG,2) << "cfdVectorBase destructor"
   //                       << std::endl  << vprDEBUG_FLUSH;

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
   //May be removed because of new vecommand code
   return false;
}

void cfdVectorBase::UpdateCommand()
{
   //Call base method - currently does nothing
   cfdObjects::UpdateCommand();

   //Extract the specific commands from the overall command
   VE_XML::DataValuePairWeakPtr activeModelDVP = veCommand->GetDataValuePair( "Sub-Dialog Settings" );
   VE_XML::Command* objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );

   //Extract the plane position
   activeModelDVP = objectCommand->GetDataValuePair( "Position" );
   double planePosition;
   activeModelDVP->GetData( planePosition );
   SetRequestedValue( static_cast< int >( planePosition ) );

   activeModelDVP = objectCommand->GetDataValuePair( "Plane Option" );
   if ( activeModelDVP )
   {
      std::string preCalculatedFlag;
      activeModelDVP->GetData( preCalculatedFlag );

      if ( preCalculatedFlag == "Use Nearest Precomputed Plane" )
      {
         SetPreCalcFlag( true );
      }
   }
   else
   {
      SetPreCalcFlag( false );
   }

   //Extract the advanced settings from the commands
   activeModelDVP = objectCommand->GetDataValuePair( "Advanced Vector Settings" );
   objectCommand = dynamic_cast< VE_XML::Command* >( activeModelDVP->GetDataXMLObject() );

   activeModelDVP = objectCommand->GetDataValuePair( "Vector Threshold" );
   std::vector< double > threshHoldValues;
   activeModelDVP->GetData( threshHoldValues );
   SetThreshHoldPercentages( static_cast< int >( threshHoldValues.at( 0 ) ),
                             static_cast< int >( threshHoldValues.at( 1 ) ) );
   UpdateThreshHoldValues();

   activeModelDVP = objectCommand->GetDataValuePair( "Vector Scale" );
   double vectorScale;
   activeModelDVP->GetData( vectorScale );
   SetVectorScale( static_cast< float >( vectorScale ) );

   activeModelDVP = objectCommand->GetDataValuePair( "Vector Ratio" );
   double vectorRatio;
   activeModelDVP->GetData( vectorRatio );
   SetVectorRatioFactor( static_cast< int >( vectorRatio ) );

   activeModelDVP = objectCommand->GetDataValuePair( "Scale By Magnitude" );
   unsigned int scaleByMagnitude;
   activeModelDVP->GetData( scaleByMagnitude );
   SetScaleByVectorFlag( static_cast< int >( scaleByMagnitude ) );
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

/*
   float range = 2.5;
   float scaleFactor = exp( this->GetVectorScale() / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetMeanCellLength();
*/
   // This scale returns a range of ~ 0.024' -> 0.75'
   float range = 2.5;
   float scaleFactor = ( exp( this->GetVectorScale() / ( 100.0 / range ) ) ) * 0.30f; 



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
      vprDEBUG(vesDBG,1) << "|\tcfdVectorBase::UpdateThreshHoldValues currentScalarRange = " 
         << currentScalarRange[ 0 ] << " : " <<  currentScalarRange[ 1 ] 
         << std::endl << vprDEBUG_FLUSH;

      _vectorThreshHoldValues[ 0 ] = currentScalarRange[0] +
                    (double)_vectorThreshHoldMinPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      _vectorThreshHoldValues[ 1 ] = currentScalarRange[0] +
                    (double)_vectorThreshHoldMaxPercentage / 100.0
                    * ( currentScalarRange[1] - currentScalarRange[0] );

      vprDEBUG(vesDBG, 1) << "|\tcfdVectorBase::UpdateThreshHoldValues Calculated Threshold Values = "
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

