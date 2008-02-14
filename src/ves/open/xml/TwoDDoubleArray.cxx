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

#include <ves/open/xml/TwoDDoubleArray.h>
#include <ves/open/xml/OneDDoubleArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
TwoDDoubleArray::TwoDDoubleArray( unsigned int nElements )
        : XMLObject()
{
    mNElements  = nElements;
    // These should match the schema for min and max occurances
    // of the float array
    mMinIndex = 2;
    SetObjectType( "TwoDDoubleArray" );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
TwoDDoubleArray::~TwoDDoubleArray()
{
    mOneDArray.clear();
}
///////////////////////////////////////////
TwoDDoubleArray::TwoDDoubleArray( const TwoDDoubleArray& input )
        : XMLObject( input )
{
    mNElements  = input.mNElements;
    for( size_t i = 0; i < input.mOneDArray.size(); ++i )
    {
        mOneDArray.push_back( new OneDDoubleArray( *( input.mOneDArray.at( i ) ) ) );
    }
    mMinIndex = input.mMinIndex;
}
/////////////////////////////////////////////////////
TwoDDoubleArray& TwoDDoubleArray::operator=( const TwoDDoubleArray& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mNElements = input.mNElements;
        mMinIndex = input.mMinIndex;

        mOneDArray.clear();

        for( size_t i = 0; i < input.mOneDArray.size(); ++i )
        {
            mOneDArray.push_back( new OneDDoubleArray( *( input.mOneDArray.at( i ) ) ) );
        }
    }
    return *this;
}
/////////////////////////////////////////////////
void TwoDDoubleArray::AddElementToArray( std::vector< double > value )
{
    mOneDArray.push_back( new OneDDoubleArray( ) );
    mOneDArray.back()->SetArray( value );
    mNElements = static_cast< unsigned int >( mOneDArray.size() );
}
/////////////////////////////////////////////////
void TwoDDoubleArray::AddElementToArray( OneDDoubleArrayPtr value )
{
    mOneDArray.push_back( value );
    mNElements = static_cast< unsigned int >( mOneDArray.size() );
}
/////////////////////////////////////////////////////////////////
void TwoDDoubleArray::SetArray( std::vector< std::vector< double > > input )
{
    mNElements = static_cast< unsigned int >( input.size() );
    // Clean old vector int
    mOneDArray.clear();

    for( size_t i = 0; i < input.size(); ++i )
    {
        mOneDArray.push_back( new OneDDoubleArray( ) );
        mOneDArray.back()->SetArray( input.at( i ) );
    }
}
//////////////////////////////////////////////////
double TwoDDoubleArray::GetElement( unsigned int i, unsigned int j )
{
    try
    {
        return mOneDArray.at( i )->GetArray().at( j );
    }
    catch ( ... )
    {
        std::cout << " ERROR!!! " << std::endl
        << " Invalid index: " << i << "," << j << " in TwoDDoubleArray::GetElement!!!" << std::endl;
        return 0;
    }
}
///////////////////////////////////////////////////
std::vector< std::vector< double > > TwoDDoubleArray::GetArray( void )
{
    std::vector< std::vector< double > > tempData;
    for( size_t i = 0; i < mOneDArray.size(); ++i )
    {
        tempData.push_back( mOneDArray.at( i )->GetArray() );
    }

    return tempData;
}
////////////////////////////////////
void TwoDDoubleArray::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren)
    //either here or in the updating subElements code
    //this will be based on the size of the double array
    //_nChildren = static_cast< unsigned int >( mOneDArray.size() );

    //Add code here to update the specific sub elements
    // This acutally needs to be an array of 1d arrays
    for( size_t i = 0; i < mOneDArray.size(); ++i )
    {
        // name comes from verg.xsd
        mOneDArray.at( i )->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mOneDArray.at( i )->GetXMLData( "index2" ) );
    }
}
////////////////////////////////////////////////////////////
void TwoDDoubleArray::SetObjectFromXMLData( DOMNode* xmlInput )
{
    //TODO:fill in the values for the double array
    //this is currently maxed out at 4 in the schema but
    //we can adjust this to be larger if needed. Also it
    //has to be at least 2 elements according to the schema
    //mNElements = xerces->();
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( xmlInput );
    }

    if( currentElement )
    {
        mOneDArray.clear();

        // do we need to delete the old one or does xerces handle this???
        //mNElements = xmlInput->getChildNodes()->getLength();
        DOMNodeList* nodeList = currentElement->getElementsByTagName( ves::open::xml::Convert( "index2" ).toXMLString() );
        XMLSize_t numNodes = nodeList->getLength();
        mNElements = numNodes;
        if( mMinIndex > numNodes )
        {
            std::cerr << " ERROR : TwoDDoubleArray::SetObjectFromXMLData :" <<
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
            mOneDArray.push_back( new OneDDoubleArray( ) );
            mOneDArray.back()->SetObjectFromXMLData( nodeList->item( i ) );
        }
    }
    else
    {
        std::cerr << " ERROR : TwoDDoubleArray::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}

