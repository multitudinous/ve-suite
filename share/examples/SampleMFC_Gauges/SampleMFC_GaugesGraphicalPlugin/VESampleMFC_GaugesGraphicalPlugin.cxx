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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VESampleMFC_GaugesGraphicalPlugin.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Conductor/Framework/string_ops.h"
#include "VE_Xplorer/cfdCursor.h"
#include "VE_Xplorer/cfdDigitalAnalogGauge.h"

#include <fstream>
//#include <cstdlib>
#include <string>
#include <map>

#include <vrj/Util/Debug.h>

#include <vtkSphereSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>

using namespace std;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// Constructor
VESampleMFC_GaugesGraphicalPlugin::VESampleMFC_GaugesGraphicalPlugin( void ) : cfdVEBaseClass()
{
  _objectName ="SampleMFC_Gauges"; // Needs to match plugin name
   //_onSceneGraph = false;
  _param.clear();
}

// Destructor
VESampleMFC_GaugesGraphicalPlugin::~VESampleMFC_GaugesGraphicalPlugin( void )
{
   if ( !_param.empty() )
      _param.clear();
}

void VESampleMFC_GaugesGraphicalPlugin::InitializeNode( cfdDCS* veworldDCS )
{
   int gauge_type = 1;
   cfdVEBaseClass::InitializeNode( veworldDCS );
   _param.assign( "./Plugins/vrxpr.param" );
   cfdGroup* rootNode = (cfdGroup*)(veworldDCS->GetParent( 0 ));
   gauge = new cfdDigitalAnalogGauge( rootNode );
   gauge->SetGaugeName( "Sample MFC Display" );
	//gauge->SetPosition( -17.0f, -19.0f, 23.0f );
   gauge->SetPosition( -5.0f, -20.0f, 20.0f );
	gauge->SetOrientation( 0.0, 90.0, 0.0 );
	gauge->SetDigitalPrecision( 3 );
	gauge->Display(gauge_type );
   gauge->SetAnalogLimits( -1000, 1000 );
   CreateObjects();
}

void VESampleMFC_GaugesGraphicalPlugin::CreateCustomVizFeature( int input ) 
{

   if ( v_value.empty() )
      return;

   double calc1 = 0;
   double calc2 = 0;
   double calc3 = 0;

   for ( unsigned int i = 0; i < v_desc.size(); i++ )
   {
      if( v_desc[ i ] == std::string( "Calculation1" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from Excel " << var << endl;
         string_to_double( var, calc1 );
      }
      if( v_desc[ i ] == std::string( "Calculation2" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from Excel " << var << endl;
         string_to_double( var, calc2 );
      }
      if( v_desc[ i ] == std::string( "Calculation3" ) )
      {
         const string var( v_value[ i ].c_str() );
         cout << " Result from Excel " << var << endl;
         string_to_double( var, calc3 );
      }
   }

   long calcselect = myInterface.getInt("int1");
   std::cout<< "CalcSelect: " << calcselect <<std::endl;
   std::cout<< "Calc1: " << calc1 <<std::endl;
   std::cout<< "Calc2: " << calc2 <<std::endl;
   std::cout<< "Calc3: " << calc3 <<std::endl;

   if ( calcselect == 0 )
   {
      gauge->UpdateMovingArrowInRange( calc1 );
	   gauge->UpdateDigitalText( calc1 );
   }
   else if ( calcselect == 1 )
   {
      gauge->UpdateMovingArrowInRange( calc2 );
	   gauge->UpdateDigitalText( calc2 );
   }
   else if ( calcselect == 2 )
   {
      gauge->UpdateMovingArrowInRange( calc3 );
	   gauge->UpdateDigitalText( calc3 );
   }

}

