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
 * File:          $RCSfile: cfdFILE.cxx,v $
 * Date modified: $Date: 2004/04/30 13:00:44 $
 * Version:       $Revision: 1.27 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdIHCCModel.h"

#include "cfdFileInfo.h"
#include <vtkActorToPF.h>
//#include <assert.h>
#include <vector>
#include <utility>
#include <sstream>
using namespace std;

#include <Performer/pfdu.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pr.h>
#include <Performer/pfutil.h>
#include <Performer/pr/pfTexture.h>
#include <Performer/pr/pfLPointState.h>
#include <Performer/pf/pfTraverser.h>
#include <Performer/pf/pfSequence.h>

#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkProperty.h>
#include <vpr/Util/Debug.h>

#include "cfdIHCCGauge.h"
#include "cfdIHCCContour.h"
#include "cfd1DTextInput.h"


cfdIHCCModel::cfdIHCCModel( fileInfo* paramFile, pfDCS* worldDCS )
{
	variables[ 0 ] = 200;  //Agitation (rpm)  initial value 200
	variables[ 1 ] = 1.25; //Air Concentration initial value 1.25;
	variables[ 2 ] = 6;    //Initial pH value    initial value 6
	variables[ 3 ] = 0.1;  //Nitrate Concentration     initial value 0.1
	variables[ 4 ] = 37;   //Temperate (Celsius)        initial value 37
	variables[ 5 ] = 240;  //Simulate [a text box] Hours in 10 seconds, initial value 240

	lut = vtkLookupTable::New();
	definedRange[ 0 ] = definedRange[ 1 ] = 0;
	pData = vtkPolyData::New();
   
#ifndef _USE_CFD_SEQUENCE
   sequence = new pfSequence();
#else
   sequence = new cfdSequence();
#endif
   ihccModelNode = new pfGroup();

   worldDCS->addChild( this->sequence );
   worldDCS->addChild( ihccModelNode );
   this->SetpfSequence( sequence );
   float scale_gauge[ 3 ];
   scale_gauge[ 0 ]  = 70;
   scale_gauge[ 1 ]  = 70;
   scale_gauge[ 2 ]  = 70;
   float trans_gauge[ 3 ];
   trans_gauge[ 0 ] = -0.5;
   trans_gauge[ 1 ] = -5;
   trans_gauge[ 2 ] = 2;
   float rot_gauge[ 3 ];
   rot_gauge[ 0 ] = 0;
   rot_gauge[ 1 ] = 60;
   rot_gauge[ 2 ] = 0;

   float scale[ 3 ];
   scale[ 0 ] = 0.0007;
   scale[ 1 ] = 0.0007;
   scale[ 2 ] = 0.0007;
   float trans[ 3 ];
   trans[ 0 ] = -0.0025;
   trans[ 1 ] = 0.0014;
   trans[ 2 ] = 0.00009;
   float rot[ 3 ];
   rot[ 0 ] = 0;
   rot[ 1 ] = 0;
   rot[ 2 ] = 0;

   float scale_nums[ 3 ];
   scale_nums[ 0 ] = 0.0008;
   scale_nums[ 1 ] = 0.0008;
   scale_nums[ 2 ] = 0.0008;
   float trans_nums[ 3 ];
   trans_nums[ 0 ] = -0.0008;
   trans_nums[ 1 ] = -0.0002;
   trans_nums[ 2 ] = 0.0002;
   float rot_nums[ 3 ];
   rot_nums[ 0 ] = 0;
   rot_nums[ 1 ] = 0;
   rot_nums[ 2 ] = 0;

   // Create two gauges
   // Time Gauge
   gauge_time = new cfdIHCCGauge( ihccModelNode );
   gauge_time->SetpfSequence( sequence );
   gauge_time->SetDCS( worldDCS );
   gauge_time->SetTranslationArray( trans_gauge );
   gauge_time->SetScaleArray( scale_gauge );
   gauge_time->SetRotationArray( rot_gauge );

   gauge_time->_textOutput.first->SetTransforms( scale, trans, rot );
   gauge_time->UpdateTransforms( scale_nums, trans_nums, rot_nums );
   gauge_time->SetUnitsTag( std::string("(Hrs)") );
   gauge_time->SetGaugeName( std::string("Time") );
   gauge_time->CreateGaugeName();
   gauge_time->SetGeometryFilename( std::string("dash_digital.flt") );

   // Acid Gauge
   trans_gauge[ 0 ] = 0.5;
   trans_gauge[ 1 ] = -5;
   trans_gauge[ 2 ] = 2;
   gauge_acid = new cfdIHCCGauge( ihccModelNode );
   gauge_acid->SetpfSequence( sequence );
   gauge_acid->SetDCS( worldDCS );
   gauge_acid->SetTranslationArray( trans_gauge );
   gauge_acid->SetScaleArray( scale_gauge );
   gauge_acid->SetRotationArray( rot_gauge );

   gauge_acid->_textOutput.first->SetTransforms( scale, trans, rot );
   gauge_acid->UpdateTransforms( scale_nums, trans_nums, rot_nums );
   gauge_acid->SetUnitsTag( std::string(" ") );
   gauge_acid->SetGaugeName( std::string("ACID") );
   gauge_acid->CreateGaugeName();
   gauge_acid->SetGeometryFilename( std::string("dash_digital.flt") );
   
   
   // Create Contours
   contours = new cfdIHCCContour();
   contours->SetpfSequence( sequence );
   contours->SetDCS( worldDCS );
}

cfdIHCCModel::~cfdIHCCModel( void )
{
}

// Update variables passed in from the gui
void cfdIHCCModel::UpdateModelVariables( double* input )
{
	for ( int i = 0; i < 6; i++ )
	{
		variables[ i ] = input[ i ];
      std::cout<< variables[ i ] << std::endl;
	}
}

// Update variables passed in from the gui
void cfdIHCCModel::RemoveSequence( void )
{
   this->StoppfSequence();
   this->contours->ClearpfSequence(); // Clears Geodes also
   this->gauge_time->ClearpfSequence(); // Clears Geodes also
   this->gauge_acid->ClearpfSequence(); // Clears Geodes also
}

void cfdIHCCModel::RunModel( void )
{
   double t;                        //time (in hours)

   double c[ 8 ];
   //array of equation answers
   double r = variables[ 0 ];
   //Defines the agitiation (in rpm) in the fermentor
   double a = variables[ 1 ];
   //defines the concentration of air initially
   double p = variables[ 2 ];
   //defines the initial pH in the fermentor
   double n = variables[ 3 ];
   //defines the initial nitrate concentration
   double k = variables[ 4 ];
   //defines the initial temperature in celsius
   double numsteps = variables[ 5 ];
   //defines the number of iterations to perform
   min = 1000000000;
   max = -1000000000;
   solutions.clear();
   times.clear();
   // Geodes have already been cleared
   int numSequenceChildren = this->sequence->getNumChildren();
   vprDEBUG(vprDBG_ALL,1) << " numSequenceChildren: " << numSequenceChildren
                          << std::endl << vprDEBUG_FLUSH;


   if ( numSequenceChildren > 0 )
   {
      for ( int i = numSequenceChildren-1; i >= 0; i-- )
      {
         // transient sequences have groups attached directly to sequence nodes
         pfGroup* group = (pfGroup*)this->sequence->getChild( i );
         this->sequence->removeChild( group );
         pfDelete( group );
      }
   }


   for(t=0;t<numsteps;t++)         //=0.4)
   {
      c[ 0 ] = 1;
      c[1] = (-0.000036*t*t*t) + (0.0092*t*t) - (0.072*t) + 1;
   { // Move this out of the for loop because they aren't dependent on t
      c[2] = (-0.000091*r*r) + 0.035*r -2.56;
      c[3] = (-1*a*a) + (2*a) -2;
      c[4] = (-0.41*p*p) + (4.9*p) - 13;
      c[5] = (-17*n*n) + (8.4*n) - 0.004;
      c[6] = (-0.01*k*k) + (0.69*k) - 7.8;
      c[7] = -1;
      c[0] = 1;
      //cout << c[ 0 ] << " : " << c[ 1 ] << " : " << c[ 2 ] << " : " 
      //               << c[ 3 ] << " : " << c[ 4 ] << " : " << c[ 5 ] << " : " << c[ 6 ] << " : " << c[ 7 ] <<endl;
   }

      for( int i=1;i<8;i++)
      {
         c[0] = c[0] * c[i];
      }

	  if ( c[ 0 ] < min )
	  {
	     min = c[ 0 ];
	  }
	  
	  if ( c[ 0 ] > max )
	  {
	     max = c[ 0 ];
	  }
      //cout << "I calculated the concentration : " << c[ 0 ] << endl;
      times.push_back( t * 10 );
//      cout << "Timestep " << t << endl;
		solutions.push_back( c[ 0 ] );
      this->sequence->addChild( new pfGroup() );
   }
   definedRange[ 0 ] = min;
   definedRange[ 1 ] = max;
}

void cfdIHCCModel::MakeLookupTable( void )
{
   // set up the vtkLookupTable
   this->lut->SetNumberOfColors( 256 );            //default is 256
   this->lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
   this->lut->SetTableRange( this->definedRange );
   this->lut->Build();
}

void cfdIHCCModel::Update( void )
{
   this->StoppfSequence();
   this->contours->ClearpfSequence(); // Clears Geodes also
   this->gauge_time->ClearSequence(); // Clears Geodes also
   this->gauge_acid->ClearSequence(); // Clears Geodes also
   this->RunModel();

   this->contours->SetDataVector( this->solutions, definedRange );
   this->contours->Update();
   this->contours->AddTopfSequence();

   this->gauge_time->SetDataVector( this->times );
   this->gauge_time->Update();
   //this->gauge_time->AddTopfSequence();

   this->gauge_acid->SetDataVector( this->solutions );
   this->gauge_acid->Update();
   //this->gauge_acid->AddTopfSequence();
   
#ifndef _USE_CFD_SEQUENCE
   this->sequence->setDuration( 1.0, -1 );
#else
   this->sequence->setDuration( 1.0);
#endif
    this->StartpfSequence();
}

void cfdIHCCModel::MakeSequence( void )
{
	vector< pfGeode* > geodes;
	vector< pfGeode* > scalars;
   vector< cfd1DTextInput* > output;
	// Loop over all the time steps and create the actors and geodes
		// read polydata
      // Translate 5 in y direction
	for ( int i = 0; i < variables[ 5 ]; i++ )
	{
      this->mapper->SetInput( pData );
      this->mapper->SetScalarRange( definedRange );
      this->mapper->SetLookupTable( lut );
	   this->mapper->SetColorModeToMapScalars();
	   this->mapper->Update();

      this->actor->SetMapper( this->mapper );
	   this->actor->GetProperty()->SetSpecularPower( 20.0f );
      this->actor->GetProperty()->SetColor( this->lut->GetColor( solutions[ i ] ) );
	   geodes.push_back( vtkActorToPF( actor, new pfGeode(), 0) );
	}
	// Add the Geodes to the sequence 
	// Add the gauges with ??? as the quantity
	for ( int i = 0; i < variables[ 5 ]; i++ )
	{
		// Convert scalar value to text
      double time = i * 10;

      std::ostringstream dataStream;
      std::string dataString;

      dataStream << time;
      dataString = dataStream.str();

      //output.push_back( new cfd1DTextInput() );
      //output.back->SetFilename( dataString );
      //output.back->SetTransforms( scale, trans, rot );
      //output.back->Update();
      //(this->sequence->getChild( i ))->addChild( output.back->getpfDCS() );
      //(this->sequence->getChild( i ))->addChild( output.back->getpfDCS() );

      // Display time and concentration
		// Create geode
		// Add geode to vector
	}
}
