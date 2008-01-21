/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <iostream>
#include <cstdlib>

#include <ves/open/xml/ThreeDIntArray.h>
#include <ves/open/xml/TwoDIntArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
ThreeDIntArray::ThreeDIntArray( unsigned int nElements )
        : XMLObject()
{
    _nElements  = nElements;
    // These should match the schema for min and max occurances
    // of the float array
    minIndex = 2;
    SetObjectType( "ThreeDIntArray" );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
ThreeDIntArray::~ThreeDIntArray()
{
    for( size_t i = 0; i < twoDArray.size(); ++i )
    {
        delete twoDArray.at( i );
    }
    twoDArray.clear();
}
///////////////////////////////////////////
ThreeDIntArray::ThreeDIntArray( const ThreeDIntArray& input )
        : XMLObject( input )
{
    _nElements  = input._nElements;
    for( size_t i = 0; i < input.twoDArray.size(); ++i )
    {
        twoDArray.push_back( new TwoDIntArray( *( input.twoDArray.at( i ) ) ) );
    }
    minIndex = input.minIndex;
}
/////////////////////////////////////////////////////
ThreeDIntArray& ThreeDIntArray::operator=( const ThreeDIntArray& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        _nElements = input._nElements;
        minIndex = input.minIndex;

        for( size_t i = 0; i < twoDArray.size(); ++i )
        {
            delete twoDArray.at( i );
        }
        twoDArray.clear();

        for( size_t i = 0; i < input.twoDArray.size(); ++i )
        {
            twoDArray.push_back( new TwoDIntArray( *( input.twoDArray.at( i ) ) ) );
        }
    }
    return *this;
}
/////////////////////////////////////////////////
void ThreeDIntArray::AddElementToArray( std::vector< std::vector< long > > value )
{
    twoDArray.push_back( new TwoDIntArray( ) );
    twoDArray.back()->SetArray( value );
    _nElements = static_cast< unsigned int >( twoDArray.size() );
}
/////////////////////////////////////////////////
void ThreeDIntArray::AddElementToArray( TwoDIntArray* value )
{
    twoDArray.push_back( value );
    _nElements = static_cast< unsigned int >( twoDArray.size() );
}
/////////////////////////////////////////////////////////////////
void ThreeDIntArray::SetArray( std::vector< std::vector< std::vector< long > > > input )
{
    _nElements = static_cast< unsigned int >( input.size() );
    // Clean old vector int
    for( size_t i = 0; i < twoDArray.size(); ++i )
    {
        delete twoDArray.at( i );
    }
    twoDArray.clear();

    for( size_t i = 0; i < input.size(); ++i )
    {
        twoDArray.push_back( new TwoDIntArray( ) );
        twoDArray.back()->SetArray( input.at( i ) );
    }
}
//////////////////////////////////////////////////
long ThreeDIntArray::GetElement( unsigned int i, unsigned int j, unsigned int k )
{
    try
    {
        return twoDArray.at( i )->GetElement( j, k );
    }
    catch ( ... )
    {
        std::cout << " ERROR!!! " << std::endl
        << " Invalid index: " << i << "," << j << "," << k
        << " in ThreeDIntArray::GetElement!!! " << std::endl;
        return 0;
    }
}
///////////////////////////////////////////////////
std::vector< std::vector< std::vector< long > > > ThreeDIntArray::GetArray( void )
{
    std::vector< std::vector< std::vector< long > > > tripleArray;///<Raw data.
    for( size_t i = 0; i < tripleArray.size(); ++i )
    {
        tripleArray.push_back( twoDArray.at( i )->GetArray() );
    }

    return tripleArray;
}
////////////////////////////////////
void ThreeDIntArray::_updateVEElement( std::string input )
{
    //Be sure to set the number of children (_nChildren)
    //either here or in the updating subElements code
    //this will be based on the size of the double array
    //_nChildren = static_cast< unsigned int >( twoDArray.size() );

    //Add code here to update the specific sub elements
    // This acutally needs to be an array of 1d arrays
    for( size_t i = 0; i < twoDArray.size(); ++i )
    {
        // name comes from verg.xsd
        twoDArray.at( i )->SetOwnerDocument( _rootDocument );
        _veElement->appendChild( twoDArray.at( i )->GetXMLData( "index3" ) );
    }
}
////////////////////////////////////////////////////////////
void ThreeDIntArray::SetObjectFromXMLData( DOMNode* xmlInput )
{
    //TODO:fill in the values for the double array
    //this is currently maxed out at 4 in the schema but
    //we can adjust this to be larger if needed. Also it
    //has to be at least 2 elements according to the schema
    //_nElements = xerces->();
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( xmlInput );
    }

    if( currentElement )
    {
        for( size_t i = 0; i < twoDArray.size(); ++i )
        {
            delete twoDArray.at( i );
        }
        twoDArray.clear();

        // do we need to delete the old one or does xerces handle this???
        //_nElements = xmlInput->getChildNodes()->getLength();
        DOMNodeList* nodeList = currentElement->getElementsByTagName( xercesString( "index3" ) );
        XMLSize_t numNodes = nodeList->getLength();
        _nElements = numNodes;
        if( minIndex > numNodes )
        {
            std::cerr << " ERROR : ThreeDIntArray::SetObjectFromXMLData :" <<
            " This node has too few or too many children." << std::endl;
        }

        // This for loop may be wrong since the the text node and
        // element may be seprate entities.
        // if that is the case then inside the for loop
        // we just need to get each text node child

        // need to get 1d arrays from 1darray class and construct the raw vectors
        for( XMLSize_t i = 0; i < numNodes; ++i )
        {
            //We know this about the node so we can cast it...
            twoDArray.push_back( new TwoDIntArray( ) );
            twoDArray.back()->SetObjectFromXMLData( nodeList->item( i ) );
        }
    }
    else
    {
        std::cerr << " ERROR : ThreeDIntArray::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}

