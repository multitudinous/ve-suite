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
 * File:          $RCSfile: Link.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_XML;
using namespace VE_Model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Link::Link(  )
:XMLObject(  )
{
   portInfo.first = new DataValuePair("FLOAT" );
   portInfo.second = new DataValuePair("FLOAT" );
   SetObjectType("Link");
   SetObjectNamespace("Model");
}
///////////////////////////////////
Link::~Link()
{
   delete portInfo.first;
   delete portInfo.second;

   for ( size_t i = 0; i < linkPoints.size(); ++i )
   {
      delete linkPoints.at( i );
   }
   linkPoints.clear();
}
///////////////////////////////////////////
Link::Link( const Link& input )
:XMLObject(input)
{
   for ( size_t i = 0; i < input.linkPoints.size(); ++i )
   {
      linkPoints.push_back( new Point( *(input.linkPoints.at( i )) ) );
   }

   portInfo.first = new DataValuePair( *(input.portInfo.first) );
   portInfo.second = new DataValuePair( *(input.portInfo.second) );
}
/////////////////////////////////////////////////////
Link& Link::operator=( const Link& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      for ( size_t i = 0; i < linkPoints.size(); ++i )
      {
         delete linkPoints.at( i );
      }
      linkPoints.clear();

      for ( size_t i = 0; i < input.linkPoints.size(); ++i )
      {
         linkPoints.push_back( new Point( *(input.linkPoints.at( i )) ) );
      }

      *(portInfo.first) = *(input.portInfo.first);
      *(portInfo.second) = *(input.portInfo.second);
   }
   return *this;
}
///////////////////////////////////////
void Link::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if ( !_veElement )
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }

   // write all the elements according to verg_model.xsd
   
   SetSubElement( "fromPort", portInfo.first );
   SetSubElement( "toPort", portInfo.second );
   for ( size_t i = 0; i < linkPoints.size(); ++i )
   {
      SetSubElement( "linkPoints", linkPoints.at( i ) );   
   }
}
///////////////////////////////////////////////////
DataValuePair* Link::GetFromPort( void )
{
   return portInfo.first;
}
//////////////////////////////////////////
DataValuePair* Link::GetToPort( void )
{
   return portInfo.second;
}
/////////////////////////////////////
Point* Link::GetLinkPoint( unsigned int i )
{
   try
   {
      return linkPoints.at( i );
   }
   catch (...)
   {
      if ( i > (linkPoints.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         linkPoints.push_back( new Point(  ) );
         return linkPoints.back();
      }
   }
}
/////////////////////////////////////
size_t Link::GetNumberOfLinkPoints( void )
{
   return linkPoints.size();
}
////////////////////////////////////////////////////////////
void Link::SetObjectFromXMLData(DOMNode* element)
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
      // for port location
      {
         dataValueStringName = GetSubElement( currentElement, "fromPort", 0 );
         if ( portInfo.first )
         {
            delete portInfo.first;
            portInfo.first = 0;
         }
         portInfo.first = new VE_XML::DataValuePair( "FLOAT" );
         portInfo.first->SetObjectFromXMLData( dataValueStringName );
      }
      // for port location
      {
         dataValueStringName = GetSubElement( currentElement, "toPort", 0 );
         if ( portInfo.second )
         {
            delete portInfo.second;
            portInfo.second = 0;
         }
         portInfo.second = new VE_XML::DataValuePair( "FLOAT" );
         portInfo.second->SetObjectFromXMLData( dataValueStringName );
      }
      // for link points
      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("linkPoints") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
            linkPoints.push_back( new Point(  ) );
            linkPoints.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
   

