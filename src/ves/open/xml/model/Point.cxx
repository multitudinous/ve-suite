/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <iostream>
#include <cstdlib>

#include <ves/open/xml/model/Point.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
Point::Point()
        : XMLObject()
{
    point.first = 0;
    point.second = 0;
    SetObjectType( "Point" );
    SetObjectNamespace( "Model" );
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
        : XMLObject( input )
{
    point = input.point;
}
/////////////////////////////////////////////////////
Point& Point::operator=( const Point& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
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
    SetAttribute( "xLocation", point.first );
    SetAttribute( "yLocation", point.second );
}
////////////////////////////////////////////////////////////
void Point::SetObjectFromXMLData( DOMNode* xmlInput )
{
    //TODO:fill in the values for the double array
    //this is currently maxed out at 4 in the schema but
    //we can adjust this to be larger if needed. Also it
    //has to be at least 2 elements according to the schema
    //_nElements = xerces->();
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = static_cast< DOMElement* >( xmlInput );
    }

    if( !currentElement )
    {
        std::cerr << " ERROR : Point::SetObjectFromXMLData :" <<
        "This node has no children which means there is probably a problem."
        << std::endl;
    }

    // Let's get the X location
    DOMElement* xNode = GetSubElement( currentElement, "xLocation", 0 );
    if( xNode )
    {
        //We know this about the node so we can cast it...
        point.first = ExtractFromSimpleElement< unsigned int >( xNode );
    }
    else
    {
        GetAttribute( currentElement, "xLocation", point.first );
    }

    // Let's get the Y location
    DOMElement* yNode = GetSubElement( currentElement, "yLocation", 0 );
    if( yNode )
    {
        //We know this about the node so we can cast it...
        point.second = ExtractFromSimpleElement< unsigned int >( yNode );
    }
    else
    {
        GetAttribute( currentElement, "yLocation", point.second );
    }
}

