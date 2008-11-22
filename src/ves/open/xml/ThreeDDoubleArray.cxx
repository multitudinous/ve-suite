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

#include <ves/open/xml/ThreeDDoubleArray.h>
#include <ves/open/xml/TwoDDoubleArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
ThreeDDoubleArray::ThreeDDoubleArray( unsigned int nElements )
        : XMLObject()
{
    mNElements  = nElements;
    // These should match the schema for min and max occurances
    // of the float array
    mMinIndex = 2;
    SetObjectType( "ThreeDDoubleArray" );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
ThreeDDoubleArray::~ThreeDDoubleArray()
{
    mTwoDArray.clear();
}
///////////////////////////////////////////
ThreeDDoubleArray::ThreeDDoubleArray( const ThreeDDoubleArray& input )
        : XMLObject( input )
{
    mNElements  = input.mNElements;
    for( size_t i = 0; i < input.mTwoDArray.size(); ++i )
    {
        mTwoDArray.push_back( TwoDDoubleArrayPtr( new TwoDDoubleArray( *( input.mTwoDArray.at( i ) ) ) ) );
    }
    mMinIndex = input.mMinIndex;
}
/////////////////////////////////////////////////////
ThreeDDoubleArray& ThreeDDoubleArray::operator=( const ThreeDDoubleArray& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mNElements = input.mNElements;
        mMinIndex = input.mMinIndex;

        mTwoDArray.clear();

        for( size_t i = 0; i < input.mTwoDArray.size(); ++i )
        {
            mTwoDArray.push_back( TwoDDoubleArrayPtr( new TwoDDoubleArray( *( input.mTwoDArray.at( i ) ) ) ) );
        }
    }
    return *this;
}
/////////////////////////////////////////////////
void ThreeDDoubleArray::AddElementToArray( std::vector< std::vector< double > > value )
{
    mTwoDArray.push_back( TwoDDoubleArrayPtr( new TwoDDoubleArray( ) ) );
    mTwoDArray.back()->SetArray( value );
    mNElements = static_cast< unsigned int >( mTwoDArray.size() );
}
/////////////////////////////////////////////////
void ThreeDDoubleArray::AddElementToArray( TwoDDoubleArrayPtr value )
{
    mTwoDArray.push_back( value );
    mNElements = static_cast< unsigned int >( mTwoDArray.size() );
}
/////////////////////////////////////////////////////////////////
void ThreeDDoubleArray::SetArray( std::vector< std::vector< std::vector< double > > > input )
{
    mNElements = static_cast< unsigned int >( input.size() );
    // Clean old vector int
    mTwoDArray.clear();

    for( size_t i = 0; i < input.size(); ++i )
    {
        mTwoDArray.push_back( TwoDDoubleArrayPtr( new TwoDDoubleArray( ) ) );
        mTwoDArray.back()->SetArray( input.at( i ) );
    }
}
//////////////////////////////////////////////////
double ThreeDDoubleArray::GetElement( unsigned int i, unsigned int j, unsigned int k )
{
    try
    {
        return mTwoDArray.at( i )->GetElement( j, k );
    }
    catch ( ... )
    {
        std::cout << " ERROR!!! " << std::endl
        << " Invalid index: " << i << "," << j << "," << k
        << " in ThreeDDoubleArray::GetElement!!! " << std::endl;
        return 0;
    }
}
///////////////////////////////////////////////////
std::vector< std::vector< std::vector< double > > > ThreeDDoubleArray::GetArray( void )
{
    std::vector< std::vector< std::vector< double > > > tripleArray;///<Raw data.
    for( size_t i = 0; i < tripleArray.size(); ++i )
    {
        tripleArray.push_back( mTwoDArray.at( i )->GetArray() );
    }

    return tripleArray;
}
////////////////////////////////////
void ThreeDDoubleArray::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren)
    //either here or in the updating subElements code
    //this will be based on the size of the double array
    //_nChildren = static_cast< unsigned int >( mTwoDArray.size() );

    //Add code here to update the specific sub elements
    // This acutally needs to be an array of 1d arrays
    for( size_t i = 0; i < mTwoDArray.size(); ++i )
    {
        // name comes from verg.xsd
        mTwoDArray.at( i )->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mTwoDArray.at( i )->GetXMLData( "index3" ) );
    }
}
////////////////////////////////////////////////////////////
void ThreeDDoubleArray::SetObjectFromXMLData( DOMNode* xmlInput )
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
        mTwoDArray.clear();

        // do we need to delete the old one or does xerces handle this???
        //mNElements = xmlInput->getChildNodes()->getLength();
        DOMNodeList* nodeList = currentElement->getElementsByTagName( ves::open::xml::Convert( "index3" ).toXMLString() );
        XMLSize_t numNodes = nodeList->getLength();
        mNElements = numNodes;
        if( mMinIndex > numNodes )
        {
            std::cerr << " ERROR : ThreeDDoubleArray::SetObjectFromXMLData :" <<
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
            mTwoDArray.push_back( TwoDDoubleArrayPtr( new TwoDDoubleArray() ) );
            mTwoDArray.back()->SetObjectFromXMLData( nodeList->item( i ) );
        }
    }
    else
    {
        std::cerr << " ERROR : ThreeDDoubleArray::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}

