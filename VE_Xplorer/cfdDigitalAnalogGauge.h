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
 * File:          $RCSfile: cfdDigitalAnalogGauge.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_DIGITALANALOGGAUGE_H
#define CFD_DIGITALANALOGGAUGE_H

#include <utility>
#include <string>

class cfdGroup;
class cfdDCS;
class cfdGeode;
class vtkArrowSource;
class vtkTransform;
class vtkMatrix4x4;
class vtkTransformPolyDataFilter;
class vtkPolyDataMapper;
class vtkActor;
class vtkVectorText;

class cfdDigitalAnalogGauge
{
   public:

      cfdDigitalAnalogGauge( const char * input, cfdGroup* );
      cfdDigitalAnalogGauge( const cfdDigitalAnalogGauge& g );
      ~cfdDigitalAnalogGauge( void );

      // Set/Get the position of the gauge in 3D space
      void SetPosition( float x, float y, float z );
      void SetPosition( float x[3] );
      void GetPosition( float x[3] );
      void GetPosition( float &x, float &y, float &z );
      void SetOrientation( double Xrot, double Yrot, double Zrot );

      vtkActor * GetCircleActor();
      vtkActor * GetStationaryArrowActor();
      vtkActor * GetMovingArrowActor();
      vtkActor * GetLabelActor();
      vtkActor * GetDigitalActor();

      void Display();

      void SetDigitalPrecision( int input );
      void UpdateMovingArrowAngle( double angle );
      void UpdateDigitalText( double value );

      cfdDCS * GetGaugeNode();

   private:
   
      void DefineCircleActor();
      void DefineStationaryArrowActor();
      void DefineMovingArrowActor();
      void DefineGaugeTextActor( const char * input );
      void DefineDigitalActor();

      cfdGroup* masterNode;
      cfdDCS * gaugeDCS;

      cfdGeode* circleGeode;
      cfdGeode* movingArrowGeode;
      cfdGeode* stationaryArrowGeode;
      cfdGeode* labelGeode;
      cfdGeode* digitalGeode;

      float itsX [ 3 ];
      float zrot;
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
      vtkActor * digitalActor;
      vtkVectorText * digitalLabel;
      char digitalText [ 100 ];
      int digitalPrecision;
};

#endif

