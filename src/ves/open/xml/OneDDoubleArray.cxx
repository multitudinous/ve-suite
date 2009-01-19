/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include <ves/open/xml/OneDDoubleArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
OneDDoubleArray::OneDDoubleArray( unsigned int nElements )
        : XMLObject()
{
    // These should match the schema for min and max occurances
    // of the float array
    mMinIndex = 1;
    SetObjectType( "OneDDoubleArray" );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
OneDDoubleArray::~OneDDoubleArray()
{
    ;
}
///////////////////////////////////////////
OneDDoubleArray::OneDDoubleArray( const OneDDoubleArray& input )
        : XMLObject( input )
{
    mArray = input.mArray;
    mMinIndex = input.mMinIndex;
}
/////////////////////////////////////////////////////
OneDDoubleArray& OneDDoubleArray::operator=( const OneDDoubleArray& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mArray = input.mArray;
        mMinIndex = input.mMinIndex;
    }
    return *this;
}
/////////////////////////////////////////////////
void OneDDoubleArray::AddElementToArray( double value )
{
    mArray.push_back( value );
}
/////////////////////////////////////////////////////////////////
void OneDDoubleArray::SetArray( std::vector< double > input )
{
    mArray.clear();
    mArray = input;
}
//////////////////////////////////////////////////
double OneDDoubleArray::GetElement( unsigned int index )
{
    try
    {
        return mArray.at( index );
    }
    catch ( ... )
    {
        std::cout << "ERROR!!!" << std::endl;
        std::cout << "Invalid index: " << index << " in OneDDoubleArray::GetElement!!!" << std::endl;
        return 0;
    }
}
///////////////////////////////////////////////////
std::vector< double > OneDDoubleArray::GetArray( void )
{
    return mArray;
}
////////////////////////////////////
void OneDDoubleArray::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren)
    //either here or in the updating subElements code
    //this will be based on the size of the double array
    //_nChildren = static_cast< unsigned int >( mArray.size() );

    //Add code here to update the specific sub elements
    for( unsigned int i = 0; i < mArray.size(); ++i )
    {
        // name comes from verg.xsd
        DOMElement* valueTag  = mRootDocument->createElement(
                                Convert( "data" ).toXMLString() );

        mVeElement->appendChild( valueTag );
        DOMText* valueNum = mRootDocument->createTextNode(
                            Convert( mArray.at( i ) ).toXMLString() );

        valueTag->appendChild( valueNum );
    }
}
////////////////////////////////////////////////////////////
void OneDDoubleArray::SetObjectFromXMLData( DOMNode* xmlInput )
{
    //TODO:fill in the values for the double array
    //this is currently maxed out at 4 in the schema but
    //we can adjust this to be larger if needed. Also it
    //has to be at least 2 elements according to the schema
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( xmlInput );
    }

    if( currentElement )
    {
        mArray.clear();

        // do we need to delete the old one or does xerces handle this???
        DOMNodeList* nodeList = currentElement->getElementsByTagName(
                                Convert( "data" ).toXMLString() );

        XMLSize_t numNodes = nodeList->getLength();
        if (( mMinIndex > numNodes ) )
        {
            std::cerr << " ERROR : OneDDoubleArray::SetObjectFromXMLData :" <<
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
            char* tempString = XMLString::transcode( temp->getData() );
            std::string stringVal( tempString );
            mArray.push_back( std::atof( stringVal.c_str() ) );
            XMLString::release( &tempString );
        }
    }
    else
    {
        std::cerr << " ERROR : OneDDoubleArray::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}

