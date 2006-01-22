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
 * File:          $RCSfile: Port.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/DataValuePair.h"
#include <iostream>
#include <sstream>

using namespace VE_XML;
using namespace VE_Model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Port::Port( DOMDocument* rootDoc )
:XMLObject( rootDoc )
{
   portNumber = 0;
   modelName = '\0';
   dataFlow = '\0';
   portLocation = new Point( rootDoc );
}
///////////////////////////////////
Port::~Port()
{
   delete portLocation;

   for ( size_t i; i < portData.size(); ++i )
   {
      delete portData.at( i );
   }
   portData.clear();
}
///////////////////////////////////////////
Port::Port( const Port& input )
:XMLObject(input)
{
   for ( size_t i; i < portData.size(); ++i )
   {
      delete portData.at( i );
   }
   portData.clear();

   for ( size_t i; i < input.portData.size(); ++i )
   {
      portData.push_back( new DataValuePair( *(input.portData.at( i )) ) );
   }

   portNumber = input.portNumber;
   modelName = input.modelName;
   dataFlow = input.dataFlow;
   portLocation = new Point( *(input.portLocation) );
}
/////////////////////////////////////////////////////
Port& Port::operator=( const Port& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      for ( size_t i; i < portData.size(); ++i )
      {
         delete portData.at( i );
      }
      portData.clear();

      for ( size_t i; i < input.portData.size(); ++i )
      {
         portData.push_back( new DataValuePair( *(input.portData.at( i )) ) );
      }

      portNumber = input.portNumber;
      modelName = input.modelName;
      dataFlow = input.dataFlow;
      *portLocation = *(input.portLocation);
   }
   return *this;
}
////////////////////////////////////////////
void Port::SetPortNumber( unsigned int number )
{
   portNumber = number;
}
/////////////////////////////////////////////////////
void Port::SetModelName( std::string name )
{
   modelName = name;
}
///////////////////////////////////////////////
void Port::SetDataFlowDirection( std::string direction )
{
   dataFlow = direction;
}
////////////////////////////////////////////////////////////////////////
void Port::SetPortLocation( Point* location )
{
   // we can do this because a point is always created be default
   delete portLocation;
   portLocation = location;
}
///////////////////////////////////////////////////////////////////
void Port::SetPortData( std::vector< DataValuePair* > data )
{
   portData = data;
}
///////////////////////////////////////
void Port::_updateVEElement( std::string input )
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
unsigned int Port::GetPortNumber( void )
{
   return portNumber;
}
//////////////////////////////////////////
std::string Port::GetModelName( void )
{
   return modelName;
}
//////////////////////////////////////////
std::string Port::GetDataFlowDirection( void )
{
   return dataFlow;
}
////////////////////////////////////////////
Point* Port::GetPortLocation( void )
{
   return portLocation;
}
/////////////////////////////////////
std::vector< VE_XML::DataValuePair* > Port::GetPortData( void )
{
   return portData;
}
////////////////////////////////////////////////////////////
void Port::SetObjectFromXMLData(DOMNode* element)
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
         portLocation = new Point( _rootDocument );
         portLocation->SetObjectFromXMLData( dataValueStringName );
      }
      // for port data
      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("portData") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "portData", i );
            portData.push_back( new DataValuePair( _rootDocument ) );
            portData.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
   

