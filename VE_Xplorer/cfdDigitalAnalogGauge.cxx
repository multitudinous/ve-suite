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
 * File:          $RCSfile: cfdDigitalAnalogGauge.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdDigitalAnalogGauge.h"
#include "cfd1DTextInput.h"
#include "cfdGroup.h"
#include "cfdNode.h"
#include "cfdDCS.h"
#include "cfdGeode.h"

#include "vtkArrowSource.h"
#include "vtkPolyDataMapper.h"
#include "vtkRenderWindow.h"
#include "vtkActor.h"
#include "vtkRenderer.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkImageMapper.h"
#include "vtkProperty.h"
#include "vtkCellArray.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkMath.h"
#include "vtkVectorText.h"

#include <vpr/Util/Debug.h>

cfdDigitalAnalogGauge::cfdDigitalAnalogGauge( const char * gaugeName,
                                              cfdGroup * groupNode )
{
   this->gaugeDCS = new cfdDCS();
   this->gaugeDCS->SetName("gauge");
   vprDEBUG(vprDBG_ALL,1) << " gauges constructor. " << std::endl << vprDEBUG_FLUSH;
   this->masterNode = groupNode;
   this->SetPosition( 4.0f, 6.0f, 8.0f );
   this->SetOrientation( 0.0, 90.0, 0.0 );
   this->circleRadius = 0.75;
   this->digitalPrecision = 3;
   strcpy( this->digitalText, "--N/A--" );
   DefineCircleActor();
   DefineStationaryArrowActor();
   DefineMovingArrowActor();
   DefineGaugeTextActor( gaugeName );
   DefineDigitalActor();
   this->lowAnalogLimit = -100.0;
   this->highAnalogLimit = 100.0;
   vprDEBUG(vprDBG_ALL,1) << " leaving gauges constructor. " << std::endl << vprDEBUG_FLUSH;

}
////////////////////////////////////////////////////////////////////////////
cfdDigitalAnalogGauge::cfdDigitalAnalogGauge(const cfdDigitalAnalogGauge& g)
{
   vprDEBUG(vprDBG_ALL,2) << "Entering cfdDigitalAnalogGauge Copy Constructor" 
                          << std::endl << vprDEBUG_FLUSH;
   gaugeDCS = new cfdDCS(*g.gaugeDCS);   
   masterNode = g.masterNode;
   circleGeode = new cfdGeode(*g.circleGeode);
   movingArrowGeode = new cfdGeode(*g.movingArrowGeode);
   stationaryArrowGeode = new cfdGeode(*g.stationaryArrowGeode);
   labelGeode = new cfdGeode(*g.labelGeode);
   digitalGeode = new cfdGeode(*g.digitalGeode);
   itsX[0] = g.itsX[0];
   itsX[1] = g.itsX[1];
   itsX[2] = g.itsX[2];

   movingArrow = vtkArrowSource::New();
   movingArrow = g.movingArrow;
   arrowTransform =vtkTransform::New();
   arrowTransform = g.arrowTransform;
   arrowRefPosition = vtkMatrix4x4::New();
   arrowRefPosition = g.arrowRefPosition;
   transformer2 = vtkTransformPolyDataFilter::New();
   transformer2 = g.transformer2;
   arrowMapper = vtkPolyDataMapper::New();
   arrowMapper = g.arrowMapper;
   arrowActor = vtkActor::New();
   arrowActor = g.arrowActor;
   stationaryArrowActor = vtkActor::New();
   stationaryArrowActor = g.stationaryArrowActor;
   labelActor = vtkActor::New();
   labelActor = g.labelActor;
   digitalActor = vtkActor::New();
   digitalActor = g.digitalActor;
   digitalLabel = vtkVectorText::New();
   digitalLabel = g.digitalLabel;
   strcpy(digitalText,g.digitalText);
   digitalPrecision = g.digitalPrecision;
   circleRadius = g.circleRadius;

   vprDEBUG(vprDBG_ALL,2) << "Leaving cfdDigitalAnalogGauge Copy Constructor" 
                          << std::endl << vprDEBUG_FLUSH;
}

cfdDigitalAnalogGauge::~cfdDigitalAnalogGauge( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdDigitalAnalogGauge Destructor" 
                          << std::endl << vprDEBUG_FLUSH;
   this->movingArrow->Delete();
   this->arrowMapper->Delete();
   this->arrowTransform->Delete();
   this->arrowRefPosition->Delete();
   this->transformer2->Delete();
   this->digitalLabel->Delete();
}

void cfdDigitalAnalogGauge::SetPosition( float x[3] )
{
   this->SetPosition( x[0], x[1], x[2] );
}

void cfdDigitalAnalogGauge::SetPosition( float x, float y, float z )
{
   this->itsX[ 0 ] = x;
   this->itsX[ 1 ] = y;
   this->itsX[ 2 ] = z;  
   this->gaugeDCS->SetTranslationArray( this->itsX );
}

void cfdDigitalAnalogGauge::GetPosition( float x[3] )
{
   this->GetPosition( x[0], x[1], x[2] );
}

void cfdDigitalAnalogGauge::GetPosition( float &x, float &y, float &z )
{
   x = this->itsX[ 0 ];
   y = this->itsX[ 1 ];
   z = this->itsX[ 2 ];
}

void cfdDigitalAnalogGauge::SetOrientation( double Xrot, double Yrot, double Zrot )
{
   float rotationArray [ 3 ];
   rotationArray[ 0 ] = Xrot;
   rotationArray[ 1 ] = Yrot;
   rotationArray[ 2 ] = Zrot;
   this->gaugeDCS->SetRotationArray( rotationArray );
}

void SetAnalogLimits( double low, double high )
{
}

/*
void cfdDigitalAnalogGauge::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;

   this->node = new cfdNode();
   this->node->LoadFile( (char*)this->_filename.c_str() );
   //this->node->flatten( 0 );
   this->AddChild( this->node );
   vprDEBUG(vprDBG_ALL,0) << " gauge geometry filename : " << _filename 
                          << std::endl << vprDEBUG_FLUSH;
   this->AddChild( ((cfd1DTextInput*)_textOutput.first) );
   this->AddChild( ((cfd1DTextInput*)_textOutput.second) );
   
   this->masterNode->AddChild( this );   
}
*/

cfdDCS * cfdDigitalAnalogGauge::GetGaugeNode()
{
   return this->gaugeDCS;
}

void cfdDigitalAnalogGauge::Display()
{
   this->circleGeode = new cfdGeode();
   this->circleGeode->TranslateTocfdGeode( this->GetCircleActor() );
   this->gaugeDCS->AddChild( this->circleGeode );
   //this->GetCircleActor()->Delete();

   this->stationaryArrowGeode = new cfdGeode();
   this->stationaryArrowGeode->TranslateTocfdGeode( this->GetStationaryArrowActor() );
   this->gaugeDCS->AddChild( this->stationaryArrowGeode );
   //this->GetStationaryArrowActor()->Delete();

   this->labelGeode = new cfdGeode();
   this->labelGeode->TranslateTocfdGeode( this->GetLabelActor() );
   this->gaugeDCS->AddChild( this->labelGeode );
   //this->GetLabelActor()->Delete();

   this->movingArrowGeode = new cfdGeode();
   this->movingArrowGeode->TranslateTocfdGeode( this->GetMovingArrowActor() );
   this->gaugeDCS->AddChild( this->movingArrowGeode );
   //this->GetMovingArrowActor()->Delete();

   this->digitalGeode = new cfdGeode();
   this->digitalGeode->TranslateTocfdGeode( this->GetDigitalActor() );
   this->gaugeDCS->AddChild( this->digitalGeode );
   //this->GetDigitalActor()->Delete();

   this->masterNode->AddChild( this->gaugeDCS );
}

void cfdDigitalAnalogGauge::DefineCircleActor()
{
   int numPts = 72;
   double Center[ 3 ];
   Center[0] = 0.0; Center[1] = 0.0; Center[2] = 0.0;

   vtkPolyData *output = vtkPolyData::New();

   vtkCellArray * newLine = vtkCellArray::New();
   newLine->Allocate(newLine->EstimateSize(1,numPts));
   newLine->InsertNextCell(numPts+1);
   for ( int i = 0; i < numPts; i++ )
   {
      newLine->InsertCellPoint(i);
   }
   newLine->InsertCellPoint(0); //close the polyline
   output->SetLines(newLine);
   newLine->Delete();

/*
   // for a filled polygon...
   vtkCellArray * newPoly = vtkCellArray::New();
   newPoly->Allocate(newPoly->EstimateSize(1,numPts));
   newPoly->InsertNextCell(numPts);
   for ( int i = 0; i < numPts; i++ )
   {
      newPoly->InsertCellPoint(i);
   }
   output->SetPolys(newPoly);
   newPoly->Delete();
*/

   // define x- and y- unit vectors in plane of polygon
   double px[ 3 ], py[ 3 ];
   px[ 0 ] = 1.0;
   px[ 1 ] = 0.0;
   px[ 2 ] = 0.0;

   py[ 0 ] = 0.0;
   py[ 1 ] = 1.0;
   py[ 2 ] = 0.0;

   // Now run around normal vector to produce polygon points.
   vtkPoints *newPoints = vtkPoints::New();
   newPoints->Allocate(numPts);

   double theta = 2.0 * vtkMath::DoublePi() / numPts;
   for ( int j = 0; j < numPts; j++ )
   {
      double x [ 3 ], r[ 3 ];
      for ( int i = 0; i < 3; i++ )
      {
         r[i] = px[i]*cos((double)j*theta) + py[i]*sin((double)j*theta);
         x[i] = Center[i] + this->circleRadius * r[i];
      }
      newPoints->InsertNextPoint(x);
   }

   output->SetPoints(newPoints);
   newPoints->Delete();

   vtkPolyDataMapper * circleMapper = vtkPolyDataMapper::New();
   circleMapper->SetInput( output );
   output->Delete();

   this->circleActor = vtkActor::New();
   this->circleActor->GetProperty()->SetLineWidth( 5.0 );
   this->circleActor->SetMapper( circleMapper );
   circleMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetCircleActor()
{
   return this->circleActor;
}

void cfdDigitalAnalogGauge::DefineStationaryArrowActor()
{
   vtkArrowSource *stationaryArrow = vtkArrowSource::New();
   stationaryArrow->SetTipResolution( 1 );   // 2D triangle
   stationaryArrow->SetShaftResolution( 2 ); // 2D rectangle

   vtkTransform *transform1 = vtkTransform::New();
   transform1->Translate( 0, 1.2, 0 );
   transform1->Scale( 1, 0.4, 1 );
   transform1->RotateZ( -90 );

   vtkTransformPolyDataFilter *transformer1 = vtkTransformPolyDataFilter::New();
   transformer1->SetInput( stationaryArrow->GetOutput() );
   transformer1->SetTransform( transform1 );
   transform1->Delete();
   stationaryArrow->Delete();

   vtkPolyDataMapper *stationaryArrowMapper = vtkPolyDataMapper::New();
   stationaryArrowMapper->SetInput( transformer1->GetOutput() );
   transformer1->Delete();

   this->stationaryArrowActor = vtkActor::New();
   this->stationaryArrowActor->SetMapper( stationaryArrowMapper );
   this->stationaryArrowActor->GetProperty()->SetColor( 1.0, 0.0, 0.0 );
   stationaryArrowMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetStationaryArrowActor()
{
   return this->stationaryArrowActor;
}

void cfdDigitalAnalogGauge::DefineMovingArrowActor()
{
   this->movingArrow = vtkArrowSource::New();
   this->movingArrow->SetTipResolution( 1 );    // 2D triangle
   this->movingArrow->SetShaftResolution( 2 );  // 2D rectangle
   this->movingArrow->SetTipLength( 0.6 );
   this->movingArrow->SetTipRadius( 0.07 );
   this->movingArrow->SetShaftRadius( 0.07 );

   this->arrowTransform = vtkTransform::New();
   this->arrowTransform->PostMultiply();   //override default
   this->arrowTransform->Translate( -0.3, 0, 0 );
   this->arrowTransform->RotateZ( 90 );
   this->arrowRefPosition = vtkMatrix4x4::New();
   this->arrowTransform->GetMatrix( this->arrowRefPosition );

   this->transformer2 = vtkTransformPolyDataFilter::New();
   this->transformer2->SetInput( this->movingArrow->GetOutput() );
   this->transformer2->SetTransform( this->arrowTransform );

   this->arrowMapper = vtkPolyDataMapper::New();
   this->arrowMapper->SetInput( transformer2->GetOutput() );

   this->arrowActor = vtkActor::New();
   this->arrowActor->SetMapper( this->arrowMapper );
}

vtkActor * cfdDigitalAnalogGauge::GetMovingArrowActor()
{
   return this->arrowActor;
}

void cfdDigitalAnalogGauge::UpdateMovingArrowAngle( double angle )
{
   this->arrowTransform->SetMatrix( this->arrowRefPosition );  // reset to ref
   this->arrowTransform->RotateZ( angle );  // incremental adjustment

   this->gaugeDCS->RemoveChild( this->movingArrowGeode );
   delete this->movingArrowGeode;

   this->movingArrowGeode = new cfdGeode();
   this->movingArrowGeode->TranslateTocfdGeode( this->GetMovingArrowActor() );
   this->gaugeDCS->AddChild( this->movingArrowGeode );
}

void cfdDigitalAnalogGauge::UpdateMovingArrowInRange( double value )
{
   double arrowLimitAngle = 120.0; // arrow can swing +/- this many degrees

   // slope is rise over run...
   double slope = -( 2.0 * arrowLimitAngle ) / ( this->highAnalogLimit - this->lowAnalogLimit );

   double middleOfRange = ( this->highAnalogLimit - this->lowAnalogLimit ) / 2.0;
   double normalizedValue = value - middleOfRange;
   double angle = normalizedValue * slope;

   // verify limits...
   if      ( angle < -arrowLimitAngle ) angle = -arrowLimitAngle;
   else if ( angle >  arrowLimitAngle ) angle =  arrowLimitAngle;

   this->UpdateMovingArrowAngle( angle );
}

void cfdDigitalAnalogGauge::DefineGaugeTextActor( const char * gaugeName )
{
   vtkVectorText * label = vtkVectorText::New();
   label->SetText( gaugeName );

   vtkTransform * labelTransform = vtkTransform::New();
   double labelScale = 0.15;
   labelTransform->Scale( labelScale, labelScale, labelScale );

   vtkTransformPolyDataFilter * labelFilter = vtkTransformPolyDataFilter::New();
   labelFilter->SetTransform( labelTransform );
   labelFilter->SetInput( label->GetOutput() );
   label->Delete();

   vtkPolyDataMapper * textMapper = vtkPolyDataMapper::New();
   textMapper->SetInput( labelFilter->GetOutput() );
   labelFilter->Delete();
   double * center = textMapper->GetCenter();
   //std::cout << "center: " << center[ 0 ] << "\t" << center[ 1 ] << "\t" << center[ 2 ] << std::endl;
   labelTransform->Translate(
         -center[ 0 ] / labelScale,
         -( this->circleRadius + 4.0 *  ( labelScale / 2.0 ) ) / labelScale,
         0.0 );
   labelTransform->Delete();

   this->labelActor = vtkActor::New();
   this->labelActor->SetMapper( textMapper );
   textMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetLabelActor()
{
   return this->labelActor;
}

void cfdDigitalAnalogGauge::DefineDigitalActor()
{
   this->digitalLabel = vtkVectorText::New();
   this->digitalLabel->SetText( this->digitalText );

   vtkTransform * labelTransform = vtkTransform::New();
   double labelScale = 0.20;
   labelTransform->Scale( labelScale, labelScale, labelScale );

   vtkTransformPolyDataFilter * labelFilter = vtkTransformPolyDataFilter::New();
   labelFilter->SetTransform( labelTransform );
   labelFilter->SetInput( this->digitalLabel->GetOutput() );

   vtkPolyDataMapper * textMapper = vtkPolyDataMapper::New();
   textMapper->SetInput( labelFilter->GetOutput() );
   labelFilter->Delete();
   double * center = textMapper->GetCenter();
   //std::cout << "center: " << center[ 0 ] << "\t" << center[ 1 ] << "\t" << center[ 2 ] << std::endl;
   labelTransform->Translate(
         -center[ 0 ] / labelScale,
         -( this->circleRadius + 6.5 * center[ 1 ] ) / labelScale,
         0.0 );
   labelTransform->Delete();

   this->digitalActor = vtkActor::New();
   this->digitalActor->SetMapper( textMapper );
   textMapper->Delete();
}

void cfdDigitalAnalogGauge::SetDigitalPrecision( int input )
{
   this->digitalPrecision = input;
}

void cfdDigitalAnalogGauge::UpdateDigitalText( double value )
{  
   if      ( this->digitalPrecision == 0)
      sprintf( this->digitalText, "%10.0f", value );
   else if ( this->digitalPrecision == 1)
      sprintf( this->digitalText, "%10.1f", value );
   else if ( this->digitalPrecision == 2)
      sprintf( this->digitalText, "%10.2f", value );
   else if ( this->digitalPrecision == 3)
      sprintf( this->digitalText, "%10.3f", value );
   else
      sprintf( this->digitalText, "%f", value );

   this->digitalLabel->SetText( this->digitalText );

   this->gaugeDCS->RemoveChild( this->digitalGeode );
   delete this->digitalGeode;
   
   this->digitalGeode = new cfdGeode();
   this->digitalGeode->TranslateTocfdGeode( this->GetDigitalActor() );
   this->gaugeDCS->AddChild( this->digitalGeode );
}

vtkActor * cfdDigitalAnalogGauge::GetDigitalActor()
{
   return this->digitalActor;
}

