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
 * File:          $RCSfile: cfdDigitalAnalogGauge.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdDigitalAnalogGauge.h"
#include "cfdReadParam.h"
#include "cfd1DTextInput.h"
#include "cfdGroup.h"
#include "cfdNode.h"

#include <vpr/Util/Debug.h>

#include <string>

cfdDigitalAnalogGauge::cfdDigitalAnalogGauge( cfdGroup *masterNode )
{
   _textOutput = std::make_pair( new cfd1DTextInput(), new cfd1DTextInput() );
   this->_masterNode = masterNode;
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
cfdDigitalAnalogGauge::~cfdDigitalAnalogGauge( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdDigitalAnalogGauge Destructor" 
                          << std::endl << vprDEBUG_FLUSH;

}

void cfdDigitalAnalogGauge::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;

   this->node = new cfdNode();
   this->node->LoadFile( (char*)this->_filename.c_str() );
   //this->node->flatten( 0 );
   this->AddChild( this->node );
   std::cout << "cfdExecutive load gauge geometry : " << _filename << std::endl;
   this->AddChild( ((cfd1DTextInput*)_textOutput.first) );
   this->AddChild( ((cfd1DTextInput*)_textOutput.second) );
   
   this->_masterNode->AddChild( this );   
}

void cfdDigitalAnalogGauge::SetGaugeName( std::string tagName )
{
   this->_gaugeName = tagName;
}

void cfdDigitalAnalogGauge::Update( void )
{
   _textOutput.first->Update();
   _textOutput.second->Update();
}

void cfdDigitalAnalogGauge::SetModuleName( std::string moduleName )
{
   this->_moduleName = moduleName;
}

std::string cfdDigitalAnalogGauge::GetModuleName( void )
{
   return this->_moduleName;
}

std::string cfdDigitalAnalogGauge::GetDataTag( void )
{
   return this->_gaugeTagName;
}

void cfdDigitalAnalogGauge::SetDataValue( std::string data )
{
   _textOutput.second->SetFilename( data );
}

void cfdDigitalAnalogGauge::SetUnitsTag( std::string units )
{
   _unitsName = units;
}

void cfdDigitalAnalogGauge::SetDataTag( std::string tag )
{
   _gaugeTagName = tag;
}

void cfdDigitalAnalogGauge::CreateGaugeName( void )
{
   std::string text;
   text = _gaugeName + " " + _unitsName;
   std::cout << text << std::endl;
   _textOutput.first->SetFilename( text );
}
