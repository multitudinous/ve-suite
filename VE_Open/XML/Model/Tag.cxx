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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Model/Tag.h"
#include "VE_Open/XML/Model/Point.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_XML;
using namespace VE_XML::VE_Model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Tag::Tag()
:XMLObject()
{
   SetObjectType("Tag");
   SetObjectNamespace("Model");   
}
///////////////////////////////////
Tag::~Tag()
{
   for ( size_t i = 0; i < tagPoints.size(); ++i )
   {
      delete tagPoints.at( i );
   }
   tagPoints.clear();
}
///////////////////////////////////////////
Tag::Tag( const Tag& input )
:XMLObject(input)
{
   tagText = input.tagText;

   for ( size_t i = 0; i < input.tagPoints.size(); ++i )
   {
      tagPoints.push_back( new Point( *(input.tagPoints.at( i )) ) );
   }
}
/////////////////////////////////////////////////////
Tag& Tag::operator=( const Tag& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      tagText = input.tagText;

      for ( size_t i = 0; i < tagPoints.size(); ++i )
      {
         delete tagPoints.at( i );
      }
      tagPoints.clear();

      for ( size_t i = 0; i < input.tagPoints.size(); ++i )
      {
         tagPoints.push_back( new Point( *(input.tagPoints.at( i )) ) );
      }
   }
   return *this;
}
///////////////////////////////////////////////////
void Tag::SetTagText( std::string text )
{
   tagText = text;
}
///////////////////////////////////////
void Tag::_updateVEElement( std::string input )
{
   // write all the elements according to verg_model.xsd
   SetSubElement( "tagText", tagText );
   for ( size_t i = 0; i < tagPoints.size(); ++i )
   {
      SetSubElement( "linkPoints", tagPoints.at( i ) );   
   }
}
///////////////////////////////////////////////////
std::string Tag::GetTagText( void )
{
   return tagText;
}
/////////////////////////////////////
Point* Tag::GetTagPoint( unsigned int i )
{
   try
   {
      return tagPoints.at( i );
   }
   catch (...)
   {
      if ( i > (tagPoints.size() + 1) )
      {
         std::cerr << "The element request is out of sequence."
                     << " Please ask for a lower number point." << std::endl;
         return 0;
      }
      else
      {
         tagPoints.push_back( new Point(  ) );
         return tagPoints.back();
      }
   }
}
////////////////////////////////////////////////////////////
void Tag::SetObjectFromXMLData(DOMNode* element)
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
       dataValueStringName = GetSubElement( currentElement, "tagText", 0 );
       tagText = ExtractFromSimpleElement< std::string >( dataValueStringName );
      // for Tag points
     unsigned int numberOfPoints = 
      currentElement->getElementsByTagName( 
      xercesString("linkPoints") )->getLength();

     for ( unsigned int i = 0; i < numberOfPoints; ++i )
     {
        dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
        tagPoints.push_back( new Point(  ) );
        tagPoints.back()->SetObjectFromXMLData( dataValueStringName );
     }
   }   
}
   

