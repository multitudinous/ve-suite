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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdIHCCContour.h"
#include "readWriteVtkThings.h"  // for readVtkThing
#include <vtkActorToPF.h>

using namespace std;

#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkTransformFilter.h>
#include <vtkTransform.h>
#include <vtkProperty.h>

#include <vpr/Util/Debug.h>

cfdIHCCContour::cfdIHCCContour( void )
{
	variables[ 0 ] = 200;  //Agitation (rpm)  initial value 200
	variables[ 1 ] = 1.25; //Air Concentration initial value 1.25;
	variables[ 2 ] = 6;    //Initial pH value    initial value 6
	variables[ 3 ] = 0.1;  //Nitrate Concentration     initial value 0.1
	variables[ 4 ] = 37;   //Temperate (Celsius)        initial value 37
	variables[ 5 ] = 240;  //Simulate [a text box] Hours in 10 seconds, initial value 240
	definedRange[ 0 ] = definedRange[ 1 ] = 0;
	mapper = vtkPolyDataMapper::New();
   actor = vtkActor::New();
   lut = NULL;
   // Create two gauges
}

cfdIHCCContour::~cfdIHCCContour( void )
{
}

// Update variables passed in from the gui
void cfdIHCCContour::UpdateModelVariables( double* input )
{
	for ( int i = 0; i < 6; i++ )
	{
		variables[ i ] = input[ i ];
	}
}

void cfdIHCCContour::SetDataVector( vector< double > input, double* x )
{
   this->solutions = input;
   definedRange[ 0 ] = x[ 0 ];
   definedRange[ 1 ] = x[ 1 ];
   cout << definedRange[ 0 ] << " : " << definedRange[ 1 ] << endl;
}

void cfdIHCCContour::RunModel( void )
{
	vector< double > solutions;
   double t;                        //time (in hours)
   int i;                           //looping index

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
   max = 0;
   solutions.clear();
   for(t=0;t<numsteps;t++)         //=0.4)
   {
      c[1] = -0.000036*t*t*t + 0.0092*t*t - 0.072*t + 1;
      c[2] = -0.000091*r*r + 0.035*r -2.56;
      c[3] = -1*a*a + 2*a -2;
      c[4] = -0.41*p*p + 4.9*p - 13;
      c[5] = -17*n*n + 8.4*n - 0.004;
      c[6] = -0.01*k*k + 0.69*k - 7.8;
      c[7] = -1;
      c[0] = 1;

//      cout << "Timestep " << t << endl;

      for(i=1;i<8;i++)
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
//      cout << "I calculated the concentration" << endl;
		solutions.push_back( c[ 0 ] );
   }
   definedRange[ 0 ] = min;
   definedRange[ 1 ] = max;
}

void cfdIHCCContour::MakeLookupTable( void )
{
   cout << "lut range: " << this->definedRange[ 0 ] << " : " << this->definedRange[ 1 ] << endl;
   if ( lut == NULL )
   	lut = vtkLookupTable::New();

   // set up the vtkLookupTable
   this->lut->SetNumberOfColors( 256 );            //default is 256
   this->lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
   this->lut->SetTableRange( this->definedRange );
   this->lut->Build();
}

void cfdIHCCContour::Update( void )
{
	// Loop over all the time steps and create the actors and geodes
		// read polydata
      // Translate 5 in y direction
   this->MakeLookupTable();
   vtkDataSet* pData = readVtkThing( "./Y_MultiCont_0.vtk" );
   vtkTransform* transform = vtkTransform::New();
   transform->Translate( 0, 5, 0 );
   transform->Update();

   // Transformation 
   vtkTransformFilter *transFilter = vtkTransformFilter::New();
   transFilter->SetInput( (vtkPointSet *)pData );
   transFilter->SetTransform( transform );
   transFilter->Update();
      this->mapper->SetInput( (vtkPolyData*)transFilter->GetOutput() );
      this->mapper->ScalarVisibilityOff();
      //this->mapper->SetLookupTable( lut );
	   //this->mapper->SetColorModeToMapScalars();
	   this->mapper->Update();
   //std::cout << " Contours : " << std::endl;
	for ( int i = 0; i < (int)solutions.size(); i++ )
	{
      //std::cout << i << " : " << solutions[ i ] << std::endl;

      double* color = this->lut->GetColor( solutions[ i ] );
      vtkActor* actor = vtkActor::New();
      actor->SetMapper( this->mapper );
	   actor->GetProperty()->SetSpecularPower( 20.0f );
      //cout << " Color : " << color[ 0 ] << " : " << color[ 1 ] << " : " << color[ 2 ] << endl;
      actor->GetProperty()->SetColor( color );
      pfGeode* geo = new pfGeode();
      vtkActorToPF( actor, geo, 0);
      actor->Delete();
	   geodes.push_back( geo );
	}
   this->lut->Delete();
   lut = NULL;
   pData->Delete();
   transform->Delete();
   transFilter->Delete();
}
