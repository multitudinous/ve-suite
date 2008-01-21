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
    linkName = "noName";
    moduleInfo.first = new DataValuePair( "FLOAT" );
    moduleInfo.second = new DataValuePair( "FLOAT" );
    portInfo.first = 0;
    portInfo.second = 0;
    parentModel = NULL;

    SetObjectType( "Link" );
    SetObjectNamespace( "Model" );
}
///////////////////////////////////
Link::~Link()
{
    delete moduleInfo.first;
    delete moduleInfo.second;

    for( size_t i = 0; i < linkPoints.size(); ++i )
    {
        delete linkPoints.at( i );
    }
    linkPoints.clear();
}
///////////////////////////////////////////
Link::Link( const Link& input )
        : XMLObject( input )
{
    linkName = input.linkName;
    m_type = input.m_type;
    for( size_t i = 0; i < input.linkPoints.size(); ++i )
    {
        linkPoints.push_back( new Point( *( input.linkPoints.at( i ) ) ) );
    }

    moduleInfo.first = new DataValuePair( *( input.moduleInfo.first ) );
    moduleInfo.second = new DataValuePair( *( input.moduleInfo.second ) );

    portInfo = input.portInfo;
    parentModel = input.parentModel;
}
/////////////////////////////////////////////////////
Link& Link::operator=( const Link& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        linkName = input.linkName;
        m_type = input.m_type;
        for( size_t i = 0; i < linkPoints.size(); ++i )
        {
            delete linkPoints.at( i );
        }
        linkPoints.clear();

        for( size_t i = 0; i < input.linkPoints.size(); ++i )
        {
            linkPoints.push_back( new Point( *( input.linkPoints.at( i ) ) ) );
        }

        *( moduleInfo.first ) = *( input.moduleInfo.first );
        *( moduleInfo.second ) = *( input.moduleInfo.second );
        portInfo = input.portInfo;
        linkName = input.linkName;
        parentModel = input.parentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////
void Link::SetLinkName( std::string name )
{
    linkName = name;
}
////////////////////////////////////////////////////////////
std::string Link::GetLinkName( void )
{
    return linkName;
}
///////////////////////////////////////
void Link::_updateVEElement( std::string input )
{
    // write all the elements according to verg_model.xsd
    SetAttribute( "name", linkName );
    if( !m_type.empty() )
    {
        SetAttribute( "type", m_type );
    }
    SetAttribute( "id", uuid );
    SetSubElement( "fromModule", moduleInfo.first );
    SetSubElement( "toModule", moduleInfo.second );
    SetSubElement( "fromPort", portInfo.first );
    SetSubElement( "toPort", portInfo.second );
    for( size_t i = 0; i < linkPoints.size(); ++i )
    {
        SetSubElement( "linkPoints", linkPoints.at( i ) );
    }
}
///////////////////////////////////////////////////
DataValuePair* Link::GetFromModule( void )
{
    return moduleInfo.first;
}
//////////////////////////////////////////
DataValuePair* Link::GetToModule( void )
{
    return moduleInfo.second;
}
///////////////////////////////////////////////////
long int* Link::GetFromPort( void )
{
    return &( portInfo.first );
}
//////////////////////////////////////////
long int* Link::GetToPort( void )
{
    return &( portInfo.second );
}
/////////////////////////////////////
Point* Link::GetLinkPoint( unsigned int i )
{
    try
    {
        return linkPoints.at( i );
    }
    catch ( ... )
    {
        if( i > ( linkPoints.size() + 1 ) )
        {
            std::cerr << "The element request is out of sequence."
            << " Please ask for a lower number point." << std::endl;
            return 0;
        }
        else
        {
            linkPoints.push_back( new Point() );
            return linkPoints.back();
        }
    }
}
/////////////////////////////////////
size_t Link::GetNumberOfLinkPoints( void )
{
    return linkPoints.size();
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
                uuid = tempUuid;
            }
        }
        DOMElement* dataValueStringName = 0;
        //link name
        {
            dataValueStringName = GetSubElement( currentElement, "name", 0 );
            if( dataValueStringName )
            {
                linkName = ExtractFromSimpleElement< std::string >( dataValueStringName );
                dataValueStringName = 0;
            }
            else
            {
                GetAttribute( currentElement, "name", linkName );
                if( linkName.empty() )
                {
                    linkName = "noName";
                }
            }
        }
        //link type
        {
            GetAttribute( currentElement, "type", m_type );
        }
        // for module location
        {
            dataValueStringName = GetSubElement( currentElement, "fromModule", 0 );
            if( moduleInfo.first )
            {
                delete moduleInfo.first;
                moduleInfo.first = 0;
            }
            moduleInfo.first = new ves::open::xml::DataValuePair( "FLOAT" );
            moduleInfo.first->SetObjectFromXMLData( dataValueStringName );
        }
        // for module location
        {
            dataValueStringName = GetSubElement( currentElement, "toModule", 0 );
            if( moduleInfo.second )
            {
                delete moduleInfo.second;
                moduleInfo.second = 0;
            }
            moduleInfo.second = new ves::open::xml::DataValuePair( "FLOAT" );
            moduleInfo.second->SetObjectFromXMLData( dataValueStringName );
        }

        dataValueStringName = GetSubElement( currentElement, "fromPort", 0 );
        portInfo.first = ExtractFromSimpleElement< long int >( dataValueStringName );
        dataValueStringName = GetSubElement( currentElement, "toPort", 0 );
        portInfo.second = ExtractFromSimpleElement< long int >( dataValueStringName );

        // for link points
        {
            unsigned int numberOfPortData = currentElement->getElementsByTagName( xercesString( "linkPoints" ) )->getLength();

            for( unsigned int i = 0; i < numberOfPortData; ++i )
            {
                dataValueStringName = GetSubElement( currentElement, "linkPoints", i );
                linkPoints.push_back( new Point() );
                linkPoints.back()->SetObjectFromXMLData( dataValueStringName );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetLinkType( std::string type )
{
    m_type = type;
}
////////////////////////////////////////////////////////////////////////////////
std::string Link::GetLinkType()
{
    return m_type;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetParentModel( ModelSharedPtr parent )
{
    parentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelSharedPtr Link::GetParentModel( )
{
    return parentModel;
}