/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/concept_check.hpp>

XERCES_CPP_NAMESPACE_USE
#include <iostream>

using namespace ves::open::xml;

////////////////////////////////////////////////////
ParameterBlock::ParameterBlock( unsigned int id )
    : XMLObject()
{
    mId = id;
    mDcs = TransformPtr( new Transform() );
    SetName( "NULL" );
    SetObjectType( "ParameterBlock" );
}
/////////////////////////////////////
ParameterBlock::~ParameterBlock()
{
    mProperties.clear();
}
///////////////////////////////////////////
ParameterBlock::ParameterBlock( const ParameterBlock& input )
    : XMLObject( input )
{
    mDcs = TransformPtr( new Transform( *input.mDcs ) );
    mId = input.mId;
    mParamName = input.mParamName;

    mProperties.clear();

    for( size_t i = 0; i < input.mProperties.size(); ++i )
    {
        mProperties.push_back( DataValuePairPtr( new DataValuePair( *( input.mProperties.at( i ) ) ) ) );
    }
}
/////////////////////////////////////////////////////
ParameterBlock& ParameterBlock::operator=( const ParameterBlock& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        *mDcs = *input.mDcs;
        mId = input.mId;
        mParamName = input.mParamName;

        mProperties.clear();

        for( size_t i = 0; i < input.mProperties.size(); ++i )
        {
            mProperties.push_back( DataValuePairPtr( new DataValuePair( *( input.mProperties.at( i ) ) ) ) );
        }
    }
    return *this;
}
////////////////////////////////////////////////
void ParameterBlock::SetBlockId( unsigned int id )
{
    mId = id;
}
///////////////////////////////////////////////////////////////////
void ParameterBlock::SetTransform( TransformPtr transform )
{
    mDcs = transform;
}
/////////////////////////////////////////////////////////////////
void ParameterBlock::AddProperty( DataValuePairPtr prop )
{
    mProperties.push_back( prop );
}
//////////////////////////////////////////////////////////////////
//set the data from an string representing the xml              //
//////////////////////////////////////////////////////////////////
void ParameterBlock::SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput )
{
    //this will be tricky...
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( xmlInput );
    }

    if( currentElement )
    {
        if( currentElement->hasAttribute( ves::open::xml::Convert( "id" ).toXMLString() ) )
        {
            std::string emptyCheck;
            XMLObject::GetAttribute( currentElement, "id", emptyCheck );
            if( !emptyCheck.empty() )
            {
                mUuid = emptyCheck;
            }
        }
        //get variables by tags
        DOMElement* dataValueStringName = 0;
        //get the transform
        dataValueStringName = GetSubElement( currentElement, "transform", 0 );
        mDcs = TransformPtr( new Transform() );
        mDcs->SetObjectFromXMLData( dataValueStringName );

        //Get the block id
        dataValueStringName = GetSubElement( currentElement, "blockID", 0 );
        GetDataFromElement( dataValueStringName, mId );

        //Get the block name
        dataValueStringName = GetSubElement( currentElement, "blockName", 0 );
        GetDataFromElement( dataValueStringName, mParamName );

        //Get the properties
        mProperties.clear();

        unsigned int numberOfProperties = currentElement->getElementsByTagName( ves::open::xml::Convert( "properties" ).toXMLString() )->getLength();
        for( unsigned int i = 0; i < numberOfProperties; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "properties", i );
            mProperties.push_back( DataValuePairPtr( new DataValuePair() ) );
            mProperties.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
}
////////////////////////////////////////
void ParameterBlock::_updateVEElement( const std::string& input )
{
    boost::ignore_unused_variable_warning( input );
    //Add code here to update the specific sub elements
    SetSubElement( "blockID", mId );
    SetSubElement( "blockName", mParamName );
    SetAttribute( "id", mUuid );
    DOMElement* tempElement;
    tempElement = SetSubElement<ves::open::xml::XMLObjectPtr>( "transform", mDcs );
    SetAttribute( "objectType", mDcs->GetObjectType(), tempElement );
    for( size_t i = 0; i < mProperties.size(); ++i )
    {
        tempElement = SetSubElement<ves::open::xml::XMLObjectPtr>( "properties", mProperties.at( i ) );
        SetAttribute( "objectType", mProperties.at( i )->GetObjectType(), tempElement );
    }
}
/////////////////////////////////////////
unsigned int ParameterBlock::GetBlockId()
{
    return mId;
}
/////////////////////////////////////////////////////
TransformPtr ParameterBlock::GetTransform()
{
    return mDcs;
}
///////////////////////////////////////////////////////////////////////
DataValuePairPtr ParameterBlock::GetProperty( const std::string& name )
{
    size_t nProps = mProperties.size();
    for( size_t i = 0; i < nProps; i++ )
    {
        if( mProperties.at( i )->GetDataName() == name )
        {
            return mProperties.at( i );
        }
    }
    /*
    mProperties.push_back( DataValuePairPtr( new DataValuePair() ) );
    mProperties.back()->SetDataName( name );
    */
    return DataValuePairPtr();
}
/////////////////////////////////////////////////////////////////////////
DataValuePairPtr ParameterBlock::GetProperty( int index )
{
    try
    {
        return mProperties.at( index );
    }
    catch( ... )
    {
        if( index >= 0 )
        {
            std::cerr << "The element request is out of sequence."
                      << " Please ask for a lower number point." << std::endl;
            return DataValuePairPtr();
        }
        else
        {
            mProperties.push_back( DataValuePairPtr( new DataValuePair() ) );
            return mProperties.back();
        }
    }
}
/////////////////////////////////////////////////////////////////////////
size_t ParameterBlock::GetNumberOfProperties( void )
{
    return mProperties.size();
}
/////////////////////////////////////////////////////////////////////////
void ParameterBlock::RemoveProperty( unsigned int index )
{
    if( index >= mProperties.size() )
    {
        return;
    }

    std::vector< DataValuePairPtr >::iterator iter;
    for( iter = mProperties.begin(); iter != mProperties.end(); ++iter )
    {
        if( mProperties.at( index ) == ( *iter ) )
        {
            mProperties.erase( iter );
            break;
        }
    }
}
/////////////////////////////////////////////////////////////////////////
void ParameterBlock::SetName( const std::string& name )
{
    mParamName = name;
}
/////////////////////////////////////////////////////////////////////////
std::string ParameterBlock::GetName( void )
{
    return mParamName;
}

