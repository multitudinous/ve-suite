/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Tag.h>

#include <ves/open/xml/DataValuePair.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////////////////////////////////////////   
//Constructor                             //
////////////////////////////////////////////////////////////////////////////////   
Network::Network(  )
:XMLObject(  )
{
   SetObjectType("Network");
   SetObjectNamespace("Model");
   parentModel = NULL;
}
////////////////////////////////////////////////////////////////////////////////   
Network::~Network()
{
   links.clear();

   for ( size_t i = 0; i < conductorState.size(); ++i )
   {
      delete conductorState.at( i );
   }
   conductorState.clear();
   
   tags.clear();
}
////////////////////////////////////////////////////////////////////////////////   
Network::Network( const Network& input )
:XMLObject(input)
{
   for( size_t i = 0; i < input.links.size(); ++i )
   {
      links.push_back( new Link( *(input.links.at( i )) ) );
   }

   for( size_t i = 0; i < input.conductorState.size(); ++i )
   {
      conductorState.push_back( new DataValuePair( *(input.conductorState.at( i )) ) );
   }
   
   for( size_t i = 0; i < input.tags.size(); ++i )
   {
       tags.push_back( new Tag( *input.tags.at( i ) ) );
   }
    parentModel = input.parentModel;
}
////////////////////////////////////////////////////////////////////////////////   
Network& Network::operator=( const Network& input)
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =(input);
        
        links.clear();

        for( size_t i = 0; i < input.links.size(); ++i )
        {
            links.push_back( new Link( *(input.links.at( i )) ) );
        }

        for ( size_t i = 0; i < conductorState.size(); ++i )
        {
            delete conductorState.at( i );
        }
        conductorState.clear();

        for( size_t i = 0; i < input.conductorState.size(); ++i )
        {
            conductorState.push_back( new DataValuePair( *(input.conductorState.at( i )) ) );
        }

        tags.clear();
        for( size_t i = 0; i < input.tags.size(); ++i )
        {
            tags.push_back( new Tag( *input.tags.at( i ) ) );
        }
        parentModel = input.parentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////   
void Network::_updateVEElement( std::string input )
{
    // write all the elements according to verg_model.xsd
    for ( size_t i = 0; i < links.size(); ++i )
    {
        SetSubElement( "link", &(*links.at( i )) );   
    }

    for ( size_t i = 0; i < conductorState.size(); ++i )
    {
        SetSubElement( "conductorState", conductorState.at( i ) );   
    }

    for ( size_t i = 0; i < tags.size(); ++i )
    {
        SetSubElement( "tag", &(*tags.at( i )) );   
    }
}
////////////////////////////////////////////////////////////////////////////////   
LinkWeakPtr Network::GetLink( int i )
{
   try
   {
      return links.at( i );
   }
   catch (...)
   {
       ;
   }
}
////////////////////////////////////////////////////////////////////////////////   
size_t Network::GetNumberOfLinks( void )
{
   return links.size();
}
////////////////////////////////////////////////////////////////////////////////   
DataValuePair* Network::GetDataValuePair( int i )
{
   try
   {
      return conductorState.at( i );
   }
   catch (...)
   {
      conductorState.push_back( new DataValuePair(  ) );
      return conductorState.back();
   }
}
////////////////////////////////////////////////////////////////////////////////   
void Network::SetObjectFromXMLData(DOMNode* element)
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
    
    //get variables by tags
    DOMElement* dataValueStringName = 0;
    // for link
    {
        unsigned int numberOfPortData = 
            currentElement->getElementsByTagName( 
            xercesString("link") )->getLength();

        for( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "link", i );
			ves::open::xml::model::LinkSharedPtr newLink = new Link();
			newLink->SetParentModel( parentModel );
            links.push_back( newLink );
            links.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
    // for state info
    {
        unsigned int numberOfStates = 
            currentElement->getElementsByTagName( 
            xercesString("conductorState") )->getLength();

        for( unsigned int i = 0; i < numberOfStates; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, 
                "conductorState", i );
            conductorState.push_back( new DataValuePair(  ) );
            conductorState.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
    // for tags
    {
        unsigned int numberOfPortData = 
            currentElement->getElementsByTagName( 
            xercesString("tag") )->getLength();
        for ( unsigned int i = 0; i < numberOfPortData; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "tag", i );
            tags.push_back( new Tag() );
            tags.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }      
}
////////////////////////////////////////////////////////////////////////////////   
TagPtr Network::GetTag( size_t i )
{
    try
    {
        return tags.at( i );
    }
    catch(...)
    {
        std::cerr << "Network::GetTag value greater than number of tags present"
            << std::endl;
        return TagPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////   
size_t Network::GetNumberOfTags( void )
{
    return tags.size();
}
////////////////////////////////////////////////////////////////////////////////   
void Network::AddTag( TagPtr newLink )
{
    tags.push_back( newLink );
}
////////////////////////////////////////////////////////////////////////////////   
void Network::RemoveTag( TagPtr oldLink )
{
    std::vector< TagPtr >::iterator iter;
    iter = std::find( tags.begin(), tags.end(), oldLink );
    if( iter != tags.end() )
    {
        tags.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////   
void Network::RemoveLink( LinkWeakPtr oldLink )
{
    LinkSharedPtr tempPtr = oldLink;
    std::vector< LinkSharedPtr >::iterator iter;
    iter = std::find( links.begin(), links.end(), tempPtr );
    if( iter != links.end() )
    {
        links.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////   
void Network::AddLink( LinkWeakPtr newLink )
{
    links.push_back( newLink );
}
////////////////////////////////////////////////////////////////////////////////
void Network::SetParentModel( ModelSharedPtr parent )
{
    parentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelSharedPtr Network::GetParentModel( )
{
    return parentModel;
}