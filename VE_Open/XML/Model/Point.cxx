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
 * File:          $RCSfile: Point.cxx,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <iostream>
#include <cstdlib>

#include "VE_Open/XML/Model/Point.h"
using namespace VE_XML;
using namespace VE_Model;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
Point::Point( )
:XMLObject( )
{
   point.first = 0;
   point.second = 0;
   SetObjectType("Point");
   SetObjectNamespace("Model");
}
/////////////////////////////
//Destructor               //
/////////////////////////////
Point::~Point()
{
   ;
}
///////////////////////////////////////////
Point::Point( const Point& input )
:XMLObject(input)
{
   point = input.point;
}
/////////////////////////////////////////////////////
Point& Point::operator=( const Point& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      point = input.point;
   }
   return *this;
}
/////////////////////////////////////////////////////////////////
void Point::SetPoint( std::pair< unsigned int, unsigned int > input )
{
   point = input;
}
///////////////////////////////////////////////////
std::pair< unsigned int, unsigned int > Point::GetPoint( void )
{
   return point;
}
////////////////////////////////////
void Point::_updateVEElement( std::string input )
{
   //Add code here to update the specific sub elements
   // name comes from verg.xsd
   SetSubElement( "xLocation", point.first );
   SetSubElement( "yLocation", point.second );
}
////////////////////////////////////////////////////////////
void Point::SetObjectFromXMLData(DOMNode* xmlInput)
{
   //TODO:fill in the values for the double array
   //this is currently maxed out at 4 in the schema but
   //we can adjust this to be larger if needed. Also it
   //has to be at least 2 elements according to the schema
   //_nElements = xerces->();
   DOMElement* currentElement = 0;
   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast< DOMElement* >( xmlInput );
   }
   
   if ( currentElement )
   {  
      // Let's get the X location
      DOMElement* xNode = GetSubElement( currentElement, "xLocation", 0 );
      //We know this about the node so we can cast it...
      point.first = ExtractIntegerDataNumberFromSimpleElement( xNode );

      // Let's get the Y location
      DOMElement* yNode = GetSubElement( currentElement, "yLocation", 0 );
      //We know this about the node so we can cast it...
      point.second = ExtractIntegerDataNumberFromSimpleElement( yNode );
   }
   else
   {
      std::cerr << " ERROR : Point::SetObjectFromXMLData :" << 
                  " This node has no children which means there is probably a problem." << std::endl;
   }
}

