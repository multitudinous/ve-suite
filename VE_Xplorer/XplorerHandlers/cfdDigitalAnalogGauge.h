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
#ifndef CFD_DIGITALANALOGGAUGE_H
#define CFD_DIGITALANALOGGAUGE_H
/*!\file cfdDigitalAnalogGauge.h
cfdDigitalAnalogGauge API
*/
/*!\class VE_Xplorer::cfdDigitalAnalogGauge
* 
*/
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Geode.h"

#include <utility>
#include <string>

namespace VE_SceneGraph
{
   class DCS;
	class Group;
   class Geode;
}

class vtkArrowSource;
class vtkTransform;
class vtkMatrix4x4;
class vtkTransformPolyDataFilter;
class vtkPolyDataMapper;
class vtkActor;
class vtkVectorText;

#include "VE_Installer/include/VEConfig.h"

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdDigitalAnalogGauge
   {
      public:
         ///Constructor
         cfdDigitalAnalogGauge( VE_SceneGraph::Group* );
         ///Copy Constructor
         cfdDigitalAnalogGauge( const cfdDigitalAnalogGauge& g );
         ///Destructor
         ~cfdDigitalAnalogGauge( void );

         ///Set/Get the position of the gauge in 3D space.
	      ///\param x
	      ///\param y
	      ///\param z
         void SetPosition( float x, float y, float z );
         ///Set Position.
         ///\param x[3]
	      void SetPosition( float x[3] );
         ///Get Position.
	      ///\param x[3]
         void GetPosition( float x[3] );
        
	      ///Get position.
         ///\param &x
	      ///\param &y
         ///\param &z
         void GetPosition( float &x, float &y, float &z );

         ///Set the orientation of the gauge.
	      ///\param Xrot
         ///\param Yrot
         ///\param Zrot
         void SetOrientation( double Xrot, double Yrot, double Zrot );

         ///Set the gauge name.
	      ///\param input[]
         void SetGaugeName( const char input[] );

         ///Set text2 (not sure why this is repeated)?
         ///\param input[]
         void SetText2( const char input[] );

	      ///Set the limits of the analog limits.
	      ///\param low
         ///\param high
         void SetAnalogLimits( double low, double high );

         ///Set the color.
         ///\param color[3]
         void SetColor( double color[3] );

	      ///Set the background color.
	      ///\param color[3]
         void SetBackgroundColor( double color[3] );

	      ///Display the gauge
         void Display();
 	     
         ///Display the gauge.
         ///\param markers
         void Display( int markers );

	      ///Set the precision of the digital gauge.
         ///\param input
         void SetDigitalPrecision( int input );
 
         ///Update the angle of the moving arrow.
         ///\param angle Angle of the arrow.
         void UpdateMovingArrowAngle( double angle );

         ///Update the range of the moving arrow.
         ///\param value
         void UpdateMovingArrowInRange( double value );
      
         ///Update the text of the digital gauge.
	      ///\param value
         void UpdateDigitalText( double value );
                        
         ///Return the DCS of the gauge node???
	 VE_SceneGraph::DCS* GetGaugeNode();

      private: 
          vtkActor * GetCircleActor();///<Circle for vtk actor.
          vtkActor * GetStationaryArrowActor();///<Stationary arrow for vtk actor.
          vtkActor * GetMinMarkersActor(); ///<Minimum markers for vtk actor. (angran)
          vtkActor * GetMaxMarkersActor(); ///<Max markers for vtk actor. (angran)
          vtkActor * GetMovingArrowActor();///<Moving arrows for vtk actor.
          vtkActor * GetLabelActor();///<Labels for vtk actor.
          vtkActor * GetText2Actor();///Text for vtk actor.
          vtkActor * GetDigitalActor();///<???
          vtkActor * GetBackgroundActor();///<Background for vtk actor.

      ///Define the circle actor.
      void DefineCircleActor();

      ///Define the stationary arrow actor.
      void DefineStationaryArrowActor();

      ///Define the moving arrow actor.
      void DefineMovingArrowActor();

      ///Define the gauge text actor.
      void DefineGaugeTextActor();
   
      ///Define the Text 2 actor (not sure).
      void DefineText2Actor();

      ///Define the digital actor.
      void DefineDigitalActor();

      ///Define the background actor.
      void DefineBackgroundActor();
      
      ///Define the minimum markers actor.
      void DefineMinMarkersActor();//angran

      ///Define the maximum markers actor.
      void DefineMaxMarkersActor();//angran

      osg::ref_ptr< VE_SceneGraph::Group > masterNode;///<master node ref ptr for osg.
      osg::ref_ptr< VE_SceneGraph::DCS > gaugeDCS;///<gauge DCS ref ptr for osg.

      osg::ref_ptr< VE_SceneGraph::Geode > circleGeode;///<circle geode.
      osg::ref_ptr< VE_SceneGraph::Geode > movingArrowGeode;///<moving arrow geode.
      osg::ref_ptr< VE_SceneGraph::Geode > stationaryArrowGeode;///<stationary arrow geode.
      osg::ref_ptr< VE_SceneGraph::Geode > labelGeode;///<label geode.
      osg::ref_ptr< VE_SceneGraph::Geode > text2Geode;///text2geode.
      osg::ref_ptr< VE_SceneGraph::Geode > digitalGeode;///<digital geode.
      osg::ref_ptr< VE_SceneGraph::Geode > backgroundGeode;///<background geode.
      osg::ref_ptr< VE_SceneGraph::Geode > minMarkersGeode;///<min markers. //angran
      osg::ref_ptr< VE_SceneGraph::Geode > maxMarkersGeode;///<max markers.  //angran

      float itsX [ 3 ];
      vtkArrowSource * movingArrow;///<moving arrow for vtk arrow source.
      vtkTransform * arrowTransform;///<arrow transform for vtk.
      vtkMatrix4x4 * arrowRefPosition;///<arroiw reference position for vtk.
      vtkTransformPolyDataFilter * transformer2;///<??
      vtkPolyDataMapper * arrowMapper;///<Arrow mapper for vtk.
      vtkActor * arrowActor;///<Arrow actor for vtk.
      double circleRadius;///<Circle radius.
      vtkActor * circleActor;///<Circle actor for vtk.
      vtkActor * stationaryArrowActor;///<Stationary arrow actor for vtk.
      vtkActor * labelActor;///<Label actor for vtk.
      vtkActor * text2Actor;///<???
      vtkActor * digitalActor;///<Digital actor for vtk.
      vtkActor * backgroundActor;///<Background actor for vtk.
      vtkVectorText * digitalLabel;///Digital label for vtk.
      char gaugeName [ 100 ];///<Gauge name.
      char text2 [ 100 ];///<Taxt name.
      char digitalText [ 100 ];///<Digital text.
      int digitalPrecision;///<Precision of digital gauge.
      double lowAnalogLimit, highAnalogLimit;///<Limits of analog gauge.
      double gaugeColor [ 3 ];///<Gauge color rgb values.
      double backgroundColor [ 3 ];///<Background color rgb values.
      double textScale;///<Text scale size.
      vtkActor * minMarkersActor;///<Min markers for vtk.
      vtkActor * maxMarkersActor;///<Max markers for vtk.
};
}
#endif

