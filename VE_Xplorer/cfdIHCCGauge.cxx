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
 * File:          $RCSfile: cfdIHCCGauge.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdIHCCGauge.h"
#include "cfdReadParam.h"
#include "cfd1DTextInput.h"
#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdNode.h"
#include "cfdSequence.h"
#include "cfdTempAnimation.h"

#include <vpr/Util/Debug.h>

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

cfdIHCCGauge::cfdIHCCGauge( cfdNode *masterNode )
{
   _textOutput = std::make_pair( new cfd1DTextInput(), new cfd1DTextInput() );
   this->_masterNode = (cfdGroup*)masterNode;
   //this->scale = { 0 };
   //this->trans = { 0 };
   //this->rot = { 0 };
}
/*
cfdDigitalAnalogGauge::cfdDigitalAnalogGauge( cfdDigitalAnalogGauge* x )
{
   this->DCS = x->DCS;
   this->node = x->node;
   this->filename = x->filename;

   for( int i = 0; i < 3; i++ )
   {
      this->scale[ i ] = x->scale[ i ];
      this->trans[ i ] = x->trans[ i ];
      this->rot[ i ]   = x->rot[ i ];
   }

}

*/
cfdIHCCGauge::~cfdIHCCGauge( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdDigitalAnalogGauge Destructor" 
                          << std::endl << vprDEBUG_FLUSH;
/*   if ( this->DCS != NULL )
   {
      pfDelete( this->DCS );
   }*/
}

void cfdIHCCGauge::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;

   this->node = new cfdNode();
   this->node->LoadFile( (char*)this->_filename.c_str() );
   //this->node->flatten( 0 );
   //this->node->setName("geometry");
   //Inhereted from cfdDCS
   this->AddChild( this->node );
std::cout << "cfdExecutive load gauge geometry : " << _filename << std::endl;
   _textOutput.first->Update();
   ((cfd1DTextInput*)_textOutput.first)->SetName("geometry");
   //Inhereted from cfdDCS
   this->AddChild( ((cfd1DTextInput*)_textOutput.first) );
   //this->GetPfDCS()->addChild( ((cfd1DTextInput*)_textOutput.second)->GetPfDCS() );
   
//   this->_masterNode->AddChild( this->GetDCS() );   
}

void cfdIHCCGauge::SetGaugeName( std::string tagName )
{
   this->_gaugeName = tagName;
}

void cfdIHCCGauge::SetDataVector( std::vector< double > input )
{
   this->data = input;
}

// Update variables passed in from the gui
void cfdIHCCGauge::UpdateModelVariables( double* input )
{
	for ( int i = 0; i < 6; i++ )
	{
		variables[ i ] = input[ i ];
	}
}

// Update variables passed in from the gui
void cfdIHCCGauge::UpdateTransforms( float* scale, float* trans, float* rot )
{ 
	for ( int i = 0; i < 3; i++ )
	{
		this->scale[ i ] = scale[ i ];
		this->rot[ i ] = rot[ i ];
		this->trans[ i ] = trans[ i ];
	}
}

void cfdIHCCGauge::Update( void )
{
	// Add the Geodes to the sequence 
	// Add the gauges with ??? as the quantity
   for ( int i = 0; i < (int)output.size(); i++ )
   {
      delete [] output[ i ];
   }
   
   output.clear();
//std::<<
   std::cout << " Gauges : " << std::endl;
	for ( int i = 0; i < (int)data.size(); i++ )
	{
		// Convert scalar value to text
      //double time = i * 10;

      std::ostringstream dataStream;
      std::string dataString;

      dataStream << data[ i ];
//std::cout << data[ i ] << std::endl;
      dataString = dataStream.str();

      output.push_back( new cfd1DTextInput() );
      output.back()->SetFilename( dataString );
      output.back()->SetTransforms( scale, trans, rot );
      output.back()->Update();
      cfdDCS* dcs = new cfdDCS();
      float* temp_trans = GetTranslationArray();
      float* temp_scale = GetScaleArray();
      float* temp_rot   = GetRotationArray();
      //cout <<  temp_trans[ 0 ] << " : " <<  temp_trans[ 1 ] << " : " <<  temp_trans[ 2 ] << endl;
      //cout <<  temp_scale[ 0 ] << " : " <<  temp_scale[ 1 ] << " : " <<  temp_scale[ 2 ] << endl;
      //cout <<  temp_rot[ 0 ] << " : " <<  temp_rot[ 1 ] << " : " <<  temp_rot[ 2 ] << endl;
      dcs->SetTranslationArray( temp_trans );
      dcs->SetScaleArray( temp_scale );
      dcs->SetRotationArray( temp_rot );
      dcs->AddChild( output.back() );
      //((cfdGroup*)this->GetSequence()->GetSequence()->getChild( i ))->AddChild( dcs );

      // Display time and concentration
		// Create geode
		// Add geode to vector
	}

   //_textOutput.second->Update();
}

void cfdIHCCGauge::ClearSequence( void )
{
   //this->GetSequence()->ClearSequence();
}

void cfdIHCCGauge::SetModuleName( std::string moduleName )
{
   this->_moduleName = moduleName;
}

std::string cfdIHCCGauge::GetModuleName( void )
{
   return this->_moduleName;
}

std::string cfdIHCCGauge::GetDataTag( void )
{
   return this->_gaugeTagName;
}

void cfdIHCCGauge::SetDataValue( std::string data )
{
   _textOutput.second->SetFilename( data );
}

void cfdIHCCGauge::SetUnitsTag( std::string units )
{
   _unitsName = units;
}

void cfdIHCCGauge::SetDataTag( std::string tag )
{
   _gaugeTagName = tag;
}

void cfdIHCCGauge::CreateGaugeName( void )
{
   std::string text;
   text = _gaugeName + " " + _unitsName;
   std::cout << text << std::endl;
   _textOutput.first->SetFilename( text );
}
