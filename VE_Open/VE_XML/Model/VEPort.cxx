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
 * File:          $RCSfile: VEPort.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/Model/VEPort.h"
#include "VE_Open/VE_XML/Model/VEPoint.h"
#include "VE_Open/VE_XML/VEDataValuePair.h"
#include <iostream>
#include <sstream>

using namespace VE_XML;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
VEPort::VEPort( DOMDocument* rootDoc )
:VEXMLObject( rootDoc )
{
   portNumber = 0;
   modelName = '\0';
   dataFlow = '\0';
   portLocation = new VEPoint( rootDoc );
}
///////////////////////////////////
VEPort::~VEPort()
{
   delete portLocation;

   for ( size_t i; i < portData.size(); ++i )
   {
      delete portData.at( i );
   }
   portData.clear();
}
///////////////////////////////////////////
VEPort::VEPort( const VEPort& input )
:VEXMLObject(input)
{
   for ( size_t i; i < portData.size(); ++i )
   {
      delete portData.at( i );
   }
   portData.clear();

   for ( size_t i; i < input.portData.size(); ++i )
   {
      portData.push_back( new VEDataValuePair( *(input.portData.at( i )) ) );
   }

   portNumber = input.portNumber;
   modelName = input.modelName;
   dataFlow = input.dataFlow;
   portLocation = new VEPoint( *(input.portLocation) );
}
/////////////////////////////////////////////////////
VEPort& VEPort::operator=( const VEPort& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      for ( size_t i; i < portData.size(); ++i )
      {
         delete portData.at( i );
      }
      portData.clear();

      for ( size_t i; i < input.portData.size(); ++i )
      {
         portData.push_back( new VEDataValuePair( *(input.portData.at( i )) ) );
      }

      portNumber = input.portNumber;
      modelName = input.modelName;
      dataFlow = input.dataFlow;
      *portLocation = *(input.portLocation);
   }
   return *this;
}
////////////////////////////////////////////
void VEPort::SetPortNumber( unsigned int number )
{
   portNumber = number;
}
/////////////////////////////////////////////////////
void VEPort::SetModelName( std::string name )
{
   modelName = name;
}
///////////////////////////////////////////////
void VEPort::SetDataFlowDirection( std::string direction )
{
   dataFlow = direction;
}
////////////////////////////////////////////////////////////////////////
void VEPort::SetPortLocation( VE_XML::VEPoint* location )
{
   // we can do this because a point is always created be default
   delete portLocation;
   portLocation = location;
}
///////////////////////////////////////////////////////////////////
void VEPort::SetPortData( std::vector< VE_XML::VEDataValuePair* > data )
{
   portData = data;
}
///////////////////////////////////////
void VEPort::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if ( !_veElement )
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }

   // write all the elements according to verg_model.xsd
   SetSubElement( "number", portNumber );
   SetSubElement( "name", modelName );
   SetSubElement( "dataFlow", dataFlow );
   SetSubElement( "portLocation", portLocation );
   for ( size_t i; i < portData.size(); ++i )
   {
      SetSubElement( "portData", portData.at( i ) );   
   }
}
///////////////////////////////////////////////////
unsigned int VEPort::GetPortNumber( void )
{
   return portNumber;
}
//////////////////////////////////////////
std::string VEPort::GetModelName( void )
{
   return modelName;
}
//////////////////////////////////////////
std::string VEPort::GetDataFlowDirection( void )
{
   return dataFlow;
}
////////////////////////////////////////////
VEPoint* VEPort::GetPortLocation( void )
{
   return portLocation;
}
/////////////////////////////////////
std::vector< VE_XML::VEDataValuePair* > VEPort::GetPortData( void )
{
   return portData;
}
////////////////////////////////////////////////////////////
void VEPort::SetObjectFromXMLData(DOMNode* element)
{
   DOMElement* currentElement = 0;
   if( element->getNodeType() == DOMNode::ELEMENT_NODE )
   {
      currentElement = dynamic_cast< DOMElement* >( element );
   }

   if ( currentElement )
   {
      //get variables by tags
      DOMElement* dataValueStringName = 0;
      // for port number
      {
         dataValueStringName = GetSubElement( currentElement, "number", 0 );
         portNumber = ExtractIntegerDataNumberFromSimpleElement(dataValueStringName);
      }
      // for model name
      {
         dataValueStringName = GetSubElement( currentElement, "name", 0 );
         modelName = ExtractDataStringFromSimpleElement( dataValueStringName );
      }
      // for data flow
      {
         dataValueStringName = GetSubElement( currentElement, "dataFlow", 0 );
         dataFlow = ExtractDataStringFromSimpleElement( dataValueStringName );
      }
      // for port location
      {
         dataValueStringName = GetSubElement( currentElement, "portLocation", 0 );
         if ( portLocation )
         {
            delete portLocation;
            portLocation = 0;
         }
         portLocation = new VE_XML::VEPoint( _rootDocument );
         portLocation->SetObjectFromXMLData( dataValueStringName );
      }
      // for port data
      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("portData") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "portData", i );
            portData.push_back( new VEDataValuePair( _rootDocument ) );
            portData.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
   

