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

#include <ves/open/xml/FloatArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
FloatArray::FloatArray( unsigned int nElements )
        : XMLObject()
{
    _nElements  = nElements;
    // These should match the schema for min and max occurances
    // of the float array
    minIndex = 2;
    maxIndex = 4;
    SetObjectType( "FloatArray" );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
FloatArray::~FloatArray()
{
    if( _array.size() )
        _array.clear();
}
///////////////////////////////////////////
FloatArray::FloatArray( const FloatArray& input )
        : XMLObject( input )
{
    _nElements = input._nElements;
    _array = input._array;
    minIndex = input.minIndex;
    maxIndex = input.maxIndex;
}
/////////////////////////////////////////////////////
FloatArray& FloatArray::operator=( const FloatArray& input )
{
    if( this != &input )
    {
        XMLObject::operator =( input );
        _nElements = input._nElements;
        _array = input._array;
        minIndex = input.minIndex;
        maxIndex = input.maxIndex;
    }
    return *this;
}
////////////////////////
void FloatArray::Clear()
{
    if( _array.size() )
        _array.clear();
}
/////////////////////////////////////////////////
void FloatArray::AddElementToArray( double value )
{
    _array.push_back( value );
    _nElements = static_cast< unsigned int >( _array.size() );
}
/////////////////////////////////////////////////////////////////
void FloatArray::SetArray( std::vector< double > input )
{
    if( input.size() )
    {
        _array.clear();
        _array = input;
        _nElements = static_cast< unsigned int >( _array.size() );
        ;
    }
}
//////////////////////////////////////////////////
double FloatArray::GetElement( unsigned int index )
{
    try
    {
        return _array.at( index );
    }
    catch ( ... )
    {
        std::cout << "ERROR!!!" << std::endl;
        std::cout << "Invalid index: " << index << " in FloatArray::GetElement!!!" << std::endl;
        return 0;
    }
}
///////////////////////////////////////////////////
std::vector< double > FloatArray::GetArray( void )
{
    return _array;
}
////////////////////////////////////
void FloatArray::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren)
    //either here or in the updating subElements code
    //this will be based on the size of the double array
    //_nChildren = static_cast< unsigned int >( _array.size() );

    //Add code here to update the specific sub elements
    for( unsigned int i = 0; i < _array.size(); ++i )
    {
        // name comes from verg.xsd
        DOMElement* valueTag  = _rootDocument->createElement( ves::open::xml::XMLObject::Convert( "value" ).toXMLString() );
        _veElement->appendChild( valueTag );
        DOMText* valueNum = _rootDocument->createTextNode( xercesString( _array.at( i ) ));
        valueTag->appendChild( valueNum );
    }
}
////////////////////////////////////////////////////////////
void FloatArray::SetObjectFromXMLData( DOMNode* xmlInput )
{
    //TODO:fill in the values for the double array
    //this is currently maxed out at 4 in the schema but
    //we can adjust this to be larger if needed. Also it
    //has to be at least 2 elements according to the schema
    //_nElements = xerces->();
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( currentElement )
    {
        _array.clear();

        // do we need to delete the old one or does xerces handle this???
        //_nElements = xmlInput->getChildNodes()->getLength();
        DOMNodeList* nodeList = currentElement->getElementsByTagName( ves::open::xml::XMLObject::Convert( "value" ).toXMLString() );
        XMLSize_t numNodes = nodeList->getLength();
        _nElements = numNodes;
        if (( minIndex > numNodes ) && ( maxIndex < numNodes ) )
        {
            std::cerr << " ERROR : FloatArray::SetObjectFromXMLData :" <<
            " This node has too few or too many children." << std::endl;
        }

        // This for loop may be wrong since the the text node and
        // element may be seprate entities.
        // if that is the case then inside the for loop
        // we just need to get each text node child
        for( XMLSize_t i = 0; i < numNodes; ++i )
        {
            //We know this about the node so we can cast it...
            DOMText* temp = dynamic_cast< DOMText* >( nodeList->item( i )->getFirstChild() );
            char* tempCharData = XMLString::transcode( temp->getData() );
            std::string stringVal( tempCharData );
            _array.push_back( std::atof( stringVal.c_str() ) );
            XMLString::release( &tempCharData );
        }
    }
    else
    {
        std::cerr << " ERROR : FloatArray::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}

