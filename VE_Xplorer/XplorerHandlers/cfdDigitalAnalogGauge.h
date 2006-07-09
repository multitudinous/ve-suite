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

#include <utility>
#include <string>

namespace VE_SceneGraph{
   class cfdGroup;
   class cfdDCS;
   class cfdGeode;
}
class vtkArrowSource;
class vtkTransform;
class vtkMatrix4x4;
class vtkTransformPolyDataFilter;
class vtkPolyDataMapper;
class vtkActor;
class vtkVectorText;
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdDigitalAnalogGauge
   {
      public:
         cfdDigitalAnalogGauge( VE_SceneGraph::cfdGroup* );
         cfdDigitalAnalogGauge( const cfdDigitalAnalogGauge& g );
         ~cfdDigitalAnalogGauge( void );

         // Set/Get the position of the gauge in 3D space
         void SetPosition( float x, float y, float z );
         void SetPosition( float x[3] );
         void GetPosition( float x[3] );
         void GetPosition( float &x, float &y, float &z );
         void SetOrientation( double Xrot, double Yrot, double Zrot );

         void SetGaugeName( const char input[] );
         void SetText2( const char input[] );
         void SetAnalogLimits( double low, double high );
         void SetColor( double color[3] );
         void SetBackgroundColor( double color[3] );

         void Display();
	     void Display(int );

         void SetDigitalPrecision( int input );
         void UpdateMovingArrowAngle( double angle );
		 void UpdateMovingArrowInRange( double value );
      
         void UpdateDigitalText( double value );

         VE_SceneGraph::cfdDCS * GetGaugeNode();

      private:
   
      vtkActor * GetCircleActor();
      vtkActor * GetStationaryArrowActor();
	  vtkActor * GetMinMarkersActor(); //angran
	  vtkActor * GetMaxMarkersActor(); //angran
      vtkActor * GetMovingArrowActor();
      vtkActor * GetLabelActor();
      vtkActor * GetText2Actor();
      vtkActor * GetDigitalActor();
      vtkActor * GetBackgroundActor();

      void DefineCircleActor();
      void DefineStationaryArrowActor();
      void DefineMovingArrowActor();
      void DefineGaugeTextActor();
      void DefineText2Actor();
      void DefineDigitalActor();
      void DefineBackgroundActor();
	  void DefineMinMarkersActor();//angran
	  void DefineMaxMarkersActor();//angran

		VE_SceneGraph::cfdGroup* masterNode;
		VE_SceneGraph::cfdDCS * gaugeDCS;

		VE_SceneGraph::cfdGeode* circleGeode;
		VE_SceneGraph::cfdGeode* movingArrowGeode;
		VE_SceneGraph::cfdGeode* stationaryArrowGeode;
		VE_SceneGraph::cfdGeode* labelGeode;
		VE_SceneGraph::cfdGeode* text2Geode;
		VE_SceneGraph::cfdGeode* digitalGeode;
		VE_SceneGraph::cfdGeode* backgroundGeode;
		VE_SceneGraph::cfdGeode* minMarkersGeode; //angran
		VE_SceneGraph::cfdGeode* maxMarkersGeode; //angran

      float itsX [ 3 ];
      vtkArrowSource * movingArrow;
      vtkTransform * arrowTransform;
      vtkMatrix4x4 * arrowRefPosition;
      vtkTransformPolyDataFilter * transformer2;
      vtkPolyDataMapper * arrowMapper;
      vtkActor * arrowActor;
      double circleRadius;
      vtkActor * circleActor;
      vtkActor * stationaryArrowActor;
      vtkActor * labelActor;
      vtkActor * text2Actor;
      vtkActor * digitalActor;
      vtkActor * backgroundActor;
      vtkVectorText * digitalLabel;
      char gaugeName [ 100 ];
      char text2 [ 100 ];
      char digitalText [ 100 ];
      int digitalPrecision;
      double lowAnalogLimit, highAnalogLimit;
      double gaugeColor [ 3 ];
      double backgroundColor [ 3 ];
      double textScale;
	  vtkActor * minMarkersActor;
	  vtkActor * maxMarkersActor;
};
}
#endif

