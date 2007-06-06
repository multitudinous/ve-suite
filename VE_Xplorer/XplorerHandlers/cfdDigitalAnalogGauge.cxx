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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerHandlers/cfdDigitalAnalogGauge.h"
#include "VE_Xplorer/XplorerHandlers/cfd1DTextInput.h"

#include <vtkArrowSource.h>
#include <vtkSphereSource.h> //added by angran
#include <vtkPolyDataMapper.h>
#include <vtkRenderWindow.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkImageMapper.h>
#include <vtkProperty.h>
#include <vtkCellArray.h>
#include <vtkPoints.h>
#include <vtkPolyData.h>
#include <vtkMath.h>
#include <vtkVectorText.h>
#include <sstream>
#include <iomanip>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_SceneGraph;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdDigitalAnalogGauge::cfdDigitalAnalogGauge( VE_SceneGraph::Group* groupNode )
{
   vprDEBUG(vesDBG,2) << " gauges constructor"
                          << std::endl << vprDEBUG_FLUSH;
   this->gaugeDCS = new VE_SceneGraph::DCS();
   this->gaugeDCS->SetName("gauge");
   this->masterNode = groupNode;
   this->SetPosition( 4.0f, 6.0f, 8.0f );
   this->SetOrientation( 0.0, 90.0, 0.0 );
   this->circleRadius = 0.75;
   this->digitalPrecision = 3;
   strcpy( this->gaugeName, "generic gauge" );
   strcpy( this->digitalText, "--N/A--" );
   this->lowAnalogLimit = -100.0;
   this->highAnalogLimit = 100.0;
   this->backgroundActor = NULL;

   for ( int i = 0; i < 3 ; i++ )
   {
      this->gaugeColor[ i ] = 1.0;  // white
      this->backgroundColor[ i ] = -1.0;  // not present
   }

   vprDEBUG(vesDBG,2) << " leaving gauges constructor"
                          << std::endl << vprDEBUG_FLUSH;

}
////////////////////////////////////////////////////////////////////////////
cfdDigitalAnalogGauge::cfdDigitalAnalogGauge(const cfdDigitalAnalogGauge& g)
{
   vprDEBUG(vesDBG,2) << "Entering cfdDigitalAnalogGauge Copy Constructor" 
                          << std::endl << vprDEBUG_FLUSH;
   gaugeDCS = new VE_SceneGraph::DCS(*g.gaugeDCS);   
   masterNode = g.masterNode;
	circleGeode = new VE_SceneGraph::Geode(*g.circleGeode);
   movingArrowGeode = new VE_SceneGraph::Geode(*g.movingArrowGeode);
   stationaryArrowGeode = new VE_SceneGraph::Geode(*g.stationaryArrowGeode);
   labelGeode = new VE_SceneGraph::Geode(*g.labelGeode);
   digitalGeode = new VE_SceneGraph::Geode(*g.digitalGeode);

   itsX[ 0 ] = g.itsX[ 0 ];
   itsX[ 1 ] = g.itsX[ 1 ];
   itsX[ 2 ] = g.itsX[ 2 ];

   movingArrow = vtkArrowSource::New();
   movingArrow = g.movingArrow;
   arrowTransform = vtkTransform::New();
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
   strcpy(gaugeName,g.gaugeName);
   strcpy(digitalText,g.digitalText);
   digitalPrecision = g.digitalPrecision;
   circleRadius = g.circleRadius;
   this->lowAnalogLimit = g.lowAnalogLimit;
   this->highAnalogLimit = g.highAnalogLimit;

   for ( int i = 0; i < 3 ; i++ )
   {
      this->gaugeColor[ i ] = g.gaugeColor[ i ];
      this->backgroundColor[ i ] = g.backgroundColor[ i ];
   }

   vprDEBUG(vesDBG,2) << "Leaving cfdDigitalAnalogGauge Copy Constructor" 
                          << std::endl << vprDEBUG_FLUSH;
}

cfdDigitalAnalogGauge::~cfdDigitalAnalogGauge( void )
{
   vprDEBUG(vesDBG,2) << "cfdDigitalAnalogGauge Destructor" 
                          << std::endl << vprDEBUG_FLUSH;
   this->movingArrow->Delete();
   this->arrowMapper->Delete();
   this->arrowTransform->Delete();
   this->arrowRefPosition->Delete();
   this->transformer2->Delete();
   this->digitalLabel->Delete();
}

void cfdDigitalAnalogGauge::SetGaugeName( const char input[] )
{
   strcpy( this->gaugeName, input );
}

void cfdDigitalAnalogGauge::SetPosition( double x[3] )
{
   this->SetPosition( x[0], x[1], x[2] );
}

void cfdDigitalAnalogGauge::SetPosition( double x, double y, double z )
{
   this->itsX[ 0 ] = x;
   this->itsX[ 1 ] = y;
   this->itsX[ 2 ] = z;  
   this->gaugeDCS->SetTranslationArray( this->itsX );
}

void cfdDigitalAnalogGauge::GetPosition( double x[3] )
{
   this->GetPosition( x[0], x[1], x[2] );
}

void cfdDigitalAnalogGauge::GetPosition( double &x, double &y, double &z )
{
   x = this->itsX[ 0 ];
   y = this->itsX[ 1 ];
   z = this->itsX[ 2 ];
}

void cfdDigitalAnalogGauge::SetOrientation( double Xrot, double Yrot, double Zrot )
{
   double rotationArray [ 3 ];
   rotationArray[ 0 ] = Xrot;
   rotationArray[ 1 ] = Yrot;
   rotationArray[ 2 ] = Zrot;
   this->gaugeDCS->SetRotationArray( rotationArray );
}

void cfdDigitalAnalogGauge::SetColor( double color[3] )
{
   for ( int i = 0; i < 3 ; i++ )
      this->gaugeColor[ i ] = color[ i ];
}

void cfdDigitalAnalogGauge::SetBackgroundColor( double color[3] )
{
   for ( int i = 0; i < 3 ; i++ )
      this->backgroundColor[ i ] = color[ i ];
}

void cfdDigitalAnalogGauge::SetAnalogLimits( double low, double high )
{
   this->lowAnalogLimit = low;
   this->highAnalogLimit = high;
}

VE_SceneGraph::DCS* cfdDigitalAnalogGauge::GetGaugeNode()
{
   return this->gaugeDCS.get();
}

void cfdDigitalAnalogGauge::Display()
{
   DefineCircleActor();
   this->circleGeode = new VE_SceneGraph::Geode();
   this->circleGeode->TranslateToGeode( this->GetCircleActor() );
   this->gaugeDCS->AddChild( this->circleGeode.get() );
   //this->GetCircleActor()->Delete();

   DefineStationaryArrowActor();
   this->stationaryArrowGeode = new VE_SceneGraph::Geode();
   this->stationaryArrowGeode->TranslateToGeode( this->GetStationaryArrowActor() );
   this->gaugeDCS->AddChild( this->stationaryArrowGeode.get() );
   //this->GetStationaryArrowActor()->Delete();

   DefineGaugeTextActor();
   this->labelGeode = new VE_SceneGraph::Geode();
   this->labelGeode->TranslateToGeode( this->GetLabelActor() );
   this->gaugeDCS->AddChild( this->labelGeode.get() );
   //this->GetLabelActor()->Delete();

/*   DefineText2Actor();
   this->text2Geode = new VE_SceneGraph::Geode();
   this->text2Geode->TranslateToGeode( this->GetText2Actor() );
   this->gaugeDCS->AddChild( this->text2Geode );
   //this->GetText2Actor()->Delete();
*/

   DefineMovingArrowActor();
   this->movingArrowGeode = new VE_SceneGraph::Geode();
   this->movingArrowGeode->TranslateToGeode( this->GetMovingArrowActor() );
   this->gaugeDCS->AddChild( this->movingArrowGeode.get() );
   //this->GetMovingArrowActor()->Delete();

   DefineDigitalActor();
   this->digitalGeode = new VE_SceneGraph::Geode();
   this->digitalGeode->TranslateToGeode( this->GetDigitalActor() );
   this->gaugeDCS->AddChild( this->digitalGeode.get() );
   //this->GetDigitalActor()->Delete();

   if ( backgroundColor[ 0 ] != -1.0 )
   {
      DefineBackgroundActor();
      this->backgroundGeode = new VE_SceneGraph::Geode();
      this->backgroundGeode->TranslateToGeode( this->GetBackgroundActor() );
      this->gaugeDCS->AddChild( this->backgroundGeode.get() );
      //this->GetBackgroundActor()->Delete();
   }

   this->masterNode->AddChild( this->gaugeDCS.get() );
}

void cfdDigitalAnalogGauge::DefineCircleActor()
{
   int numPts = 72;
   double Center[ 3 ];
   Center[0] = 0.0; Center[1] = 0.1; Center[2] = 0.0;

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
   this->circleActor->GetProperty()->SetColor( this->gaugeColor );
   this->circleActor->SetMapper( circleMapper );
   circleMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetCircleActor()
{
   return this->circleActor;
}

void cfdDigitalAnalogGauge::DefineBackgroundActor()
{
   int numPts = 4;

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

   // Now produce polygon points.
   vtkPoints *newPoints = vtkPoints::New();
   newPoints->Allocate(numPts);

   double x [ 3 ];
   x[ 0 ] = -0.85;
   x[ 1 ] =  1.20;
   x[ 2 ] = -0.02;   // put it just behind the gauge
   newPoints->InsertNextPoint( x );
   x[ 0 ] = -0.85;
   x[ 1 ] = -1.40;
   newPoints->InsertNextPoint( x );
   x[ 0 ] =  0.85;
   x[ 1 ] = -1.40;
   newPoints->InsertNextPoint( x );
   x[ 0 ] =  0.85;
   x[ 1 ] =  1.20;
   newPoints->InsertNextPoint( x );

   output->SetPoints(newPoints);
   newPoints->Delete();

   vtkPolyDataMapper * backgroundMapper = vtkPolyDataMapper::New();
   backgroundMapper->SetInput( output );
   output->Delete();

   this->backgroundActor = vtkActor::New();
   this->backgroundActor->GetProperty()->SetLineWidth( 5.0 );
   this->backgroundActor->GetProperty()->SetColor( this->backgroundColor );
   this->backgroundActor->GetProperty()->SetOpacity( 0.7 );
   this->backgroundActor->SetMapper( backgroundMapper );
   backgroundMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetBackgroundActor()
{
   return this->backgroundActor;
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
   this->stationaryArrowActor->GetProperty()->SetColor( 1.0, 0.0, 0.0 ); // red
   this->stationaryArrowActor->SetMapper( stationaryArrowMapper );
   stationaryArrowMapper->Delete();
}

vtkActor * cfdDigitalAnalogGauge::GetStationaryArrowActor()
{
   return this->stationaryArrowActor;
}

//added by angran
vtkActor * cfdDigitalAnalogGauge::GetMinMarkersActor()
{
   return this->minMarkersActor;
}

//added by angran
vtkActor * cfdDigitalAnalogGauge::GetMaxMarkersActor()
{
   return this->maxMarkersActor;
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
   this->arrowActor->GetProperty()->SetColor( this->gaugeColor );
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

   this->gaugeDCS->RemoveChild( this->movingArrowGeode.get() );

   this->movingArrowGeode = new VE_SceneGraph::Geode();
   this->movingArrowGeode->TranslateToGeode( this->GetMovingArrowActor() );
   this->gaugeDCS->AddChild( this->movingArrowGeode.get() );
}

void cfdDigitalAnalogGauge::UpdateMovingArrowInRange( double value )
{
   double arrowLimitAngle = 120.0; // arrow can swing +/- this many degrees

   // slope is rise over run...
   vprDEBUG(vesDBG,2) << " limits: " << this->lowAnalogLimit << " " << this->highAnalogLimit << std::endl << vprDEBUG_FLUSH;
   double slope = -( 2.0 * arrowLimitAngle ) / ( this->highAnalogLimit - this->lowAnalogLimit );

   double middleOfRange = ( this->highAnalogLimit + this->lowAnalogLimit ) / 2.0;
   vprDEBUG(vesDBG,2) << "middleOfRange: " << middleOfRange << std::endl << vprDEBUG_FLUSH; 
   double normalizedValue = value - middleOfRange;
   double angle = normalizedValue * slope;
   vprDEBUG(vesDBG,2) << "angle: " << angle << std::endl << vprDEBUG_FLUSH; 

   // verify limits...
   if      ( angle < -arrowLimitAngle ) angle = -arrowLimitAngle;
   else if ( angle >  arrowLimitAngle ) angle =  arrowLimitAngle;

   this->UpdateMovingArrowAngle( angle );
}

void cfdDigitalAnalogGauge::DefineGaugeTextActor()
{
   vtkVectorText * label = vtkVectorText::New();
   label->SetText( this->gaugeName );

   vtkTransform * labelTransform = vtkTransform::New();
   double labelScale = 0.15;
   // crude way to reduce size of text for big phrases...
   if ( strlen(this->gaugeName) > 14 )
      labelScale = 0.13;
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
         1.3 -( this->circleRadius + 4.0 *  ( labelScale / 2.0 ) ) / labelScale,
         0.0 );
   labelTransform->Delete();

   this->labelActor = vtkActor::New();
   this->labelActor->GetProperty()->SetColor( this->gaugeColor );
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
   this->digitalActor->GetProperty()->SetColor( this->gaugeColor );
   this->digitalActor->SetMapper( textMapper );
   textMapper->Delete();
}

void cfdDigitalAnalogGauge::SetDigitalPrecision( int input )
{
   this->digitalPrecision = input;
}

void cfdDigitalAnalogGauge::UpdateDigitalText( double value )
{  
   std::ostringstream dirStringStream;
   dirStringStream << std::setw(10) << std::setiosflags( std::ios_base::fixed );
   if      ( this->digitalPrecision == 0)
      dirStringStream << std::setprecision(0) << value;
   else if ( this->digitalPrecision == 1)
      dirStringStream << std::setprecision(1) << value;
   else if ( this->digitalPrecision == 2)
      dirStringStream << std::setprecision(2) << value;
   else if ( this->digitalPrecision == 3)
      dirStringStream << std::setprecision(3) << value;
   else
      dirStringStream << value;

   this->digitalLabel->SetText( dirStringStream.str().c_str() );

   this->gaugeDCS->RemoveChild( this->digitalGeode.get() );
   
   this->digitalGeode = new VE_SceneGraph::Geode();
   this->digitalGeode->TranslateToGeode( this->GetDigitalActor() );
   this->gaugeDCS->AddChild( this->digitalGeode.get() );
}

vtkActor * cfdDigitalAnalogGauge::GetDigitalActor()
{
   return this->digitalActor;
}

//added by angran
void cfdDigitalAnalogGauge::DefineMinMarkersActor()
{
	vtkSphereSource *minMarkers = vtkSphereSource::New();
	minMarkers->SetRadius (0.08);

   // x = -1*cos(30), y = -1*sin(30)
   vtkTransform *transform1 = vtkTransform::New();
   transform1->Translate( (-0.36*1.732), (-0.36), 0 );

   vtkTransformPolyDataFilter *transformer1 = vtkTransformPolyDataFilter::New();
   transformer1->SetInput( minMarkers->GetOutput() );
   transformer1->SetTransform( transform1 );
   transform1->Delete();
   minMarkers->Delete();

   vtkPolyDataMapper *minMarkersMapper = vtkPolyDataMapper::New();
   minMarkersMapper->SetInput( transformer1->GetOutput() );
   transformer1->Delete();

   this->minMarkersActor = vtkActor::New();
   this->minMarkersActor->GetProperty()->SetColor( 1.0, 0.0, 0.0 ); // red
   this->minMarkersActor->SetMapper( minMarkersMapper );
   minMarkersMapper->Delete();
}

//added by angran
void cfdDigitalAnalogGauge::DefineMaxMarkersActor()
{
	vtkSphereSource *maxMarkers = vtkSphereSource::New();
	maxMarkers->SetRadius (0.08);

   // x = -1*cos(30), y = -1*sin(30)
   vtkTransform *transform1 = vtkTransform::New();
   transform1->Translate( (0.36*1.732), (-0.36), 0 );

   vtkTransformPolyDataFilter *transformer1 = vtkTransformPolyDataFilter::New();
   transformer1->SetInput( maxMarkers->GetOutput() );
   transformer1->SetTransform( transform1 );
   transform1->Delete();
   maxMarkers->Delete();

   vtkPolyDataMapper *maxMarkersMapper = vtkPolyDataMapper::New();
   maxMarkersMapper->SetInput( transformer1->GetOutput() );
   transformer1->Delete();

   this->maxMarkersActor = vtkActor::New();
   this->maxMarkersActor->GetProperty()->SetColor( 1.0, 0.0, 0.0 ); // red
   this->maxMarkersActor->SetMapper( maxMarkersMapper );
   maxMarkersMapper->Delete();
}

//added by angran
void cfdDigitalAnalogGauge::Display(int markers)
{
   DefineCircleActor();
	this->circleGeode = new VE_SceneGraph::Geode();
   this->circleGeode->TranslateToGeode( this->GetCircleActor() );
   this->gaugeDCS->AddChild( this->circleGeode.get() );
   //this->GetCircleActor()->Delete();

   DefineStationaryArrowActor();
	this->stationaryArrowGeode = new VE_SceneGraph::Geode();
   this->stationaryArrowGeode->TranslateToGeode( this->GetStationaryArrowActor() );
   this->gaugeDCS->AddChild( this->stationaryArrowGeode.get() );
   //this->GetStationaryArrowActor()->Delete();

   DefineMinMarkersActor();
	this->minMarkersGeode = new VE_SceneGraph::Geode();
   this->minMarkersGeode->TranslateToGeode( this->GetMinMarkersActor() );
   this->gaugeDCS->AddChild( this->minMarkersGeode.get() );

   DefineMaxMarkersActor();
   this->maxMarkersGeode = new VE_SceneGraph::Geode();
   this->maxMarkersGeode->TranslateToGeode( this->GetMaxMarkersActor() );
   this->gaugeDCS->AddChild( this->maxMarkersGeode.get() );

   DefineGaugeTextActor();
   this->labelGeode = new VE_SceneGraph::Geode();
   this->labelGeode->TranslateToGeode( this->GetLabelActor() );
   this->gaugeDCS->AddChild( this->labelGeode.get() );
   //this->GetLabelActor()->Delete();

   DefineMovingArrowActor();
   this->movingArrowGeode = new VE_SceneGraph::Geode();
   this->movingArrowGeode->TranslateToGeode( this->GetMovingArrowActor() );
   this->gaugeDCS->AddChild( this->movingArrowGeode.get() );
   //this->GetMovingArrowActor()->Delete();

   DefineDigitalActor();
   this->digitalGeode = new VE_SceneGraph::Geode();
   this->digitalGeode->TranslateToGeode( this->GetDigitalActor() );
   this->gaugeDCS->AddChild( this->digitalGeode.get() );
   //this->GetDigitalActor()->Delete();

   if ( backgroundColor[ 0 ] != -1.0 )
   {
      DefineBackgroundActor();
      this->backgroundGeode = new VE_SceneGraph::Geode();
      this->backgroundGeode->TranslateToGeode( this->GetBackgroundActor() );
      this->gaugeDCS->AddChild( this->backgroundGeode.get() );
      //this->GetBackgroundActor()->Delete();
   }

   this->masterNode->AddChild( this->gaugeDCS.get() );
}
