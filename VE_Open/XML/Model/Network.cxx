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
 * File:          $RCSfile: Network.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"

using namespace VE_XML;
using namespace VE_Model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Network::Network( DOMDocument* rootDoc )
:VEXMLObject( rootDoc )
{
   ;
}
///////////////////////////////////
Network::~Network()
{
   for ( size_t i; i < links.size(); ++i )
   {
      delete links.at( i );
   }
   links.clear();
}
///////////////////////////////////////////
Network::Network( const Network& input )
:VEXMLObject(input)
{
   for ( size_t i; i < input.links.size(); ++i )
   {
      links.push_back( new Link( *(input.links.at( i )) ) );
   }
}
/////////////////////////////////////////////////////
Network& Network::operator=( const Network& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      for ( size_t i; i < links.size(); ++i )
      {
         delete links.at( i );
      }
      links.clear();

      for ( size_t i; i < input.links.size(); ++i )
      {
         links.push_back( new Link( *(input.links.at( i )) ) );
      }
   }
   return *this;
}
///////////////////////////////////////
void Network::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if ( !_veElement )
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }

   // write all the elements according to verg_model.xsd
   for ( size_t i; i < links.size(); ++i )
   {
      SetSubElement( "link", links.at( i ) );   
   }
}
/////////////////////////////////////
VE_XML::Link* Network::GetLink( unsigned int i )
{
   try
   {
      return links.at( i );
   }
   catch (...)
   {
      links.push_back( new Link( _rootDocument ) );
      return links.back();
   }
}
////////////////////////////////////////////////////////////
void Network::SetObjectFromXMLData(DOMNode* element)
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
      // for link
      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("link") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "link", i );
            links.push_back( new Link( _rootDocument ) );
            links.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
   

