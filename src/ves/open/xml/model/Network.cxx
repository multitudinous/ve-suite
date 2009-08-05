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
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/DataValuePair.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////////////////////////////////////////
Network::Network( )
        : XMLObject( )
{
    SetObjectType( "Network" );
    SetObjectNamespace( "Model" );
    mParentModel = ModelPtr();
}
////////////////////////////////////////////////////////////////////////////////
Network::~Network()
{
    for( size_t i = 0; i < mLinks.size(); ++i )
    {
        mLinks.at( i )->SetParentModel( ModelPtr() );
    }
    mLinks.clear();

    mConductorState.clear();

    mTags.clear();
}
////////////////////////////////////////////////////////////////////////////////
Network::Network( const Network& input )
        : XMLObject( input )
{
    for( size_t i = 0; i < input.mLinks.size(); ++i )
    {
        mLinks.push_back( LinkPtr( new Link( *( input.mLinks.at( i ) ) ) ) );
    }

    for( size_t i = 0; i < input.mConductorState.size(); ++i )
    {
        mConductorState.push_back( DataValuePairPtr( new DataValuePair( *( input.mConductorState.at( i ) ) ) ) );
    }

    for( size_t i = 0; i < input.mTags.size(); ++i )
    {
        mTags.push_back( TagPtr( new Tag( *input.mTags.at( i ) ) ) );
    }
    mParentModel = input.mParentModel;
}
////////////////////////////////////////////////////////////////////////////////
Network& Network::operator=( const Network& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );

        mLinks.clear();

        for( size_t i = 0; i < input.mLinks.size(); ++i )
        {
            mLinks.push_back( LinkPtr( new Link( *( input.mLinks.at( i ) ) ) ) );
        }

        mConductorState.clear();

        for( size_t i = 0; i < input.mConductorState.size(); ++i )
        {
            mConductorState.push_back( DataValuePairPtr( new DataValuePair( *( input.mConductorState.at( i ) ) ) ) );
        }

        mTags.clear();
        for( size_t i = 0; i < input.mTags.size(); ++i )
        {
            mTags.push_back( TagPtr( new Tag( *input.mTags.at( i ) ) ) );
        }
        mParentModel = input.mParentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void Network::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetSubElements( "link", mLinks );

    SetSubElements( "conductorState", mConductorState );

    SetSubElements( "tag", mTags );
}
////////////////////////////////////////////////////////////////////////////////
void Network::AddDataValuePair( ves::open::xml::DataValuePairPtr dataValuePair )
{
    mConductorState.push_back( dataValuePair );
}
////////////////////////////////////////////////////////////////////////////////
LinkPtr Network::GetLink( int i )
{
    try
    {
        return mLinks.at( i );
    }
    catch ( ... )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t Network::GetNumberOfLinks( void )
{
    return mLinks.size();
}
////////////////////////////////////////////////////////////////////////////////
DataValuePairPtr Network::GetDataValuePair( size_t index )
{
    try
    {
        return mConductorState.at( index );
    }
    catch( ... )
    {
        std::cerr << " Network::GetDataValuePair The element request "
        << "is out of sequence. Please ask for a lower number point."
        << std::endl;

        return DataValuePairPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Network::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( element );
    }

    if( !currentElement )
    {
        return;
    }

    //get variables by mTags
    DOMElement* dataValueStringName = 0;
    // for link
    {
        unsigned int numberOfPortData =
            currentElement->getElementsByTagName(
                Convert( "link" ).toXMLString() )->getLength();

        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "link", i );
            ves::open::xml::model::LinkPtr newLink( new Link() );
            newLink->SetParentModel( mParentModel.lock() );
            mLinks.push_back( newLink );
            mLinks.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
    // for state info
    {
        unsigned int numberOfStates =
            currentElement->getElementsByTagName(
                Convert( "conductorState" ).toXMLString() )->getLength();

        for( unsigned int i = 0; i < numberOfStates; ++i )
        {
            dataValueStringName = GetSubElement( currentElement,
                                                 "conductorState", i );
            mConductorState.push_back( DataValuePairPtr( new DataValuePair( ) ) );
            mConductorState.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
    // for mTags
    {
        unsigned int numberOfPortData =
            currentElement->getElementsByTagName(
                Convert( "tag" ).toXMLString() )->getLength();
        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "tag", i );
            mTags.push_back( TagPtr( new Tag() ) );
            mTags.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
TagPtr Network::GetTag( size_t i )
{
    try
    {
        return mTags.at( i );
    }
    catch ( ... )
    {
        std::cerr << "Network::GetTag value greater than number of mTags present"
        << std::endl;
        return TagPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t Network::GetNumberOfTags( void )
{
    return mTags.size();
}
////////////////////////////////////////////////////////////////////////////////
void Network::AddTag( TagPtr newLink )
{
    mTags.push_back( newLink );
}
////////////////////////////////////////////////////////////////////////////////
void Network::RemoveTag( TagPtr oldLink )
{
    std::vector< TagPtr >::iterator iter;
    iter = std::find( mTags.begin(), mTags.end(), oldLink );
    if( iter != mTags.end() )
    {
        mTags.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Network::RemoveLink( LinkPtr oldLink )
{
    LinkPtr tempPtr = oldLink;
    std::vector< LinkPtr >::iterator iter;
    iter = std::find( mLinks.begin(), mLinks.end(), tempPtr );
    if( iter != mLinks.end() )
    {
        mLinks.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Network::AddLink( LinkPtr newLink )
{
    mLinks.push_back( newLink );
}
////////////////////////////////////////////////////////////////////////////////
void Network::SetParentModel( ModelSharedPtr parent )
{
    mParentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelSharedPtr Network::GetParentModel( )
{
    return mParentModel.lock();
}
////////////////////////////////////////////////////////////////////////////////
size_t Network::GetNumberOfNetworkStates()
{
    return mConductorState.size();
}
