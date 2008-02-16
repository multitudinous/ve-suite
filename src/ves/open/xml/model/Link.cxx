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
#include <ves/open/xml/model/Link.h>

#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Link::Link()
        : XMLObject()
{
    mLinkName = "noName";
    mModuleInfo.first = new DataValuePair( "FLOAT" );
    mModuleInfo.second = new DataValuePair( "FLOAT" );
    mPortInfo.first = 0;
    mPortInfo.second = 0;
    mParentModel = NULL;

    SetObjectType( "Link" );
    SetObjectNamespace( "Model" );
}
///////////////////////////////////
Link::~Link()
{
    mModuleInfo.first = DataValuePairPtr();
    mModuleInfo.second = DataValuePairPtr();
    mLinkPoints.clear();
}
///////////////////////////////////////////
Link::Link( const Link& input )
        : XMLObject( input )
{
    mLinkName = input.mLinkName;
    mType = input.mType;

    std::copy( input.mLinkPoints.begin(),
               input.mLinkPoints.end(),
               std::back_inserter( mLinkPoints ) );

    mModuleInfo.first =  input.mModuleInfo.first;
    mModuleInfo.second =  input.mModuleInfo.second;

    mPortInfo = input.mPortInfo;
    mParentModel = input.mParentModel;
}
/////////////////////////////////////////////////////
Link& Link::operator=( const Link& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mLinkName = input.mLinkName;
        mType = input.mType;
        mLinkPoints.clear();

        std::copy( input.mLinkPoints.begin(),
                   input.mLinkPoints.end(),
                   std::back_inserter( mLinkPoints ) );

        mModuleInfo.first = input.mModuleInfo.first;
        mModuleInfo.second = input.mModuleInfo.second ;
        mPortInfo = input.mPortInfo;
        mLinkName = input.mLinkName;
        mParentModel = input.mParentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////
void Link::SetLinkName( const std::string& name )
{
    mLinkName = name;
}
////////////////////////////////////////////////////////////
const std::string& Link::GetLinkName( void )
{
    return mLinkName;
}
///////////////////////////////////////
void Link::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetAttribute( "name", mLinkName );
    if( !mType.empty() )
    {
        SetAttribute( "type", mType );
    }
    SetAttribute( "id", mUuid );
    SetSubElement<ves::open::xml::XMLObjectPtr>( "fromModule", mModuleInfo.first );
    SetSubElement<ves::open::xml::XMLObjectPtr>( "toModule", mModuleInfo.second );
    SetSubElement( "fromPort", mPortInfo.first );
    SetSubElement( "toPort", mPortInfo.second );
    SetSubElements( "linkPoints", mLinkPoints );
}
///////////////////////////////////////////////////
DataValuePairPtr Link::GetFromModule( void )
{
    return mModuleInfo.first;
}
//////////////////////////////////////////
DataValuePairPtr Link::GetToModule( void )
{
    return mModuleInfo.second;
}
///////////////////////////////////////////////////
long int* Link::GetFromPort( void )
{
    return &( mPortInfo.first );
}
//////////////////////////////////////////
long int* Link::GetToPort( void )
{
    return &( mPortInfo.second );
}
/////////////////////////////////////
PointPtr Link::GetLinkPoint( unsigned int i )
{
    try
    {
        return mLinkPoints.at( i );
    }
    catch ( ... )
    {
        if( i > ( mLinkPoints.size() + 1 ) )
        {
            std::cerr << "The element request is out of sequence."
            << " Please ask for a lower number point." << std::endl;
            return 0;
        }
        else
        {
            mLinkPoints.push_back( PointPtr( new Point() ) );
            return mLinkPoints.back();
        }
    }
}
/////////////////////////////////////
size_t Link::GetNumberOfLinkPoints( void )
{
    return mLinkPoints.size();
}
////////////////////////////////////////////////////////////
void Link::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( element );
    }

    if( currentElement )
    {
        //get variables by tags
        //Setup uuid for model element
        {
            std::string tempUuid;
            ves::open::xml::XMLObject::GetAttribute( currentElement, "id", tempUuid );
            if( !tempUuid.empty() )
            {
                mUuid = tempUuid;
            }
        }
        DOMElement* dataValueStringName = 0;
        //link name
        {
            dataValueStringName = GetSubElement( currentElement, "name", 0 );
            if( dataValueStringName )
            {
                GetDataFromElement( dataValueStringName,  mLinkName );
                dataValueStringName = 0;
            }
            else
            {
                GetAttribute( currentElement, "name", mLinkName );
                if( mLinkName.empty() )
                {
                    mLinkName = "noName";
                }
            }
        }
        //link type
        {
            GetAttribute( currentElement, "type", mType );
        }
        // for module location
        {
            dataValueStringName = GetSubElement( currentElement, "fromModule", 0 );
            mModuleInfo.first = new ves::open::xml::DataValuePair( "FLOAT" );
            mModuleInfo.first->SetObjectFromXMLData( dataValueStringName );
        }
        // for module location
        {
            dataValueStringName = GetSubElement( currentElement, "toModule", 0 );

            mModuleInfo.second = new ves::open::xml::DataValuePair( "FLOAT" );
            mModuleInfo.second->SetObjectFromXMLData( dataValueStringName );
        }

        dataValueStringName = GetSubElement( currentElement, "fromPort", 0 );
        GetDataFromElement( dataValueStringName, mPortInfo.first );

        dataValueStringName = GetSubElement( currentElement, "toPort", 0 );
        GetDataFromElement( dataValueStringName, mPortInfo.second );

        // for link points
        {
            unsigned int numberOfPortData = currentElement->getElementsByTagName(                     Convert( "linkPoints" ).toXMLString() )->getLength();

            for( unsigned int i = 0; i < numberOfPortData; ++i )
            {
                dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
                mLinkPoints.push_back( PointPtr( new Point() ) );
                mLinkPoints.back()->SetObjectFromXMLData( dataValueStringName );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetLinkType( const std::string& type )
{
    mType = type;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& Link::GetLinkType()
{
    return mType;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetParentModel( ModelPtr parent )
{
    mParentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelPtr Link::GetParentModel( )
{
    return mParentModel;
}
