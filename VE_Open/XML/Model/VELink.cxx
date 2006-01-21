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
 * File:          $RCSfile: VELink.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/Model/VELink.h"
#include "VE_Open/VE_XML/Model/VEPoint.h"
#include "VE_Open/VE_XML/VEDataValuePair.h"

using namespace VE_XML;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
VELink::VELink( DOMDocument* rootDoc )
:VEXMLObject( rootDoc )
{
   portInfo.first = new VEDataValuePair( rootDoc, "FLOAT" );
   portInfo.second = new VEDataValuePair( rootDoc, "FLOAT" );
}
///////////////////////////////////
VELink::~VELink()
{
   delete portInfo.first;
   delete portInfo.second;

   for ( size_t i; i < linkPoints.size(); ++i )
   {
      delete linkPoints.at( i );
   }
   linkPoints.clear();
}
///////////////////////////////////////////
VELink::VELink( const VELink& input )
:VEXMLObject(input)
{
   for ( size_t i; i < input.linkPoints.size(); ++i )
   {
      linkPoints.push_back( new VEPoint( *(input.linkPoints.at( i )) ) );
   }

   portInfo.first = new VEDataValuePair( *(input.portInfo.first) );
   portInfo.second = new VEDataValuePair( *(input.portInfo.second) );
}
/////////////////////////////////////////////////////
VELink& VELink::operator=( const VELink& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      for ( size_t i; i < linkPoints.size(); ++i )
      {
         delete linkPoints.at( i );
      }
      linkPoints.clear();

      for ( size_t i; i < input.linkPoints.size(); ++i )
      {
         linkPoints.push_back( new VEPoint( *(input.linkPoints.at( i )) ) );
      }

      *(portInfo.first) = *(input.portInfo.first);
      *(portInfo.second) = *(input.portInfo.second);
   }
   return *this;
}
///////////////////////////////////////
void VELink::_updateVEElement( std::string input )
{
   //fill in datavalue pairs, via xerces, according to schema
   if ( !_veElement )
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }

   // write all the elements according to verg_model.xsd
   SetSubElement( "fromPort", portInfo.first );
   SetSubElement( "toPort", portInfo.second );
   for ( size_t i; i < linkPoints.size(); ++i )
   {
      SetSubElement( "linkPoints", linkPoints.at( i ) );   
   }
}
///////////////////////////////////////////////////
VEDataValuePair* VELink::GetFromPort( void )
{
   return portInfo.first;
}
//////////////////////////////////////////
VEDataValuePair* VELink::GetToPort( void )
{
   return portInfo.second;
}
/////////////////////////////////////
VE_XML::VEPoint* VELink::GetLinkPoint( unsigned int i )
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
         linkPoints.push_back( new VEPoint( _rootDocument ) );
         return linkPoints.back();
      }
   }
}
////////////////////////////////////////////////////////////
void VELink::SetObjectFromXMLData(DOMNode* element)
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
         portInfo.first = new VE_XML::VEDataValuePair( _rootDocument, "FLOAT" );
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
         portInfo.second = new VE_XML::VEDataValuePair( _rootDocument, "FLOAT" );
         portInfo.second->SetObjectFromXMLData( dataValueStringName );
      }
      // for link points
      {
         unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString("linkPoints") )->getLength();

         for ( unsigned int i = 0; i < numberOfPortData; ++i )
         {
            dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
            linkPoints.push_back( new VEPoint( _rootDocument ) );
            linkPoints.back()->SetObjectFromXMLData( dataValueStringName );
         }
      }
   }   
}
   

