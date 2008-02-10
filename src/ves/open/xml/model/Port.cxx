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

#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/DataValuePair.h>
#include <iostream>
#include <sstream>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
Port::Port( )
        : XMLObject( )
{
    portNumber = 0;
    //modelName = '\0';
    //dataFlow = '\0';
    portLocation = new Point();
    SetObjectType( "Port" );
    SetObjectNamespace( "Model" );
}
///////////////////////////////////
Port::~Port()
{
    delete portLocation;

    for( size_t i = 0; i < portData.size(); ++i )
    {
        delete portData.at( i );
    }
    portData.clear();
}
///////////////////////////////////////////
Port::Port( const Port& input )
        : XMLObject( input )
{
    for( size_t i = 0; i < portData.size(); ++i )
    {
        delete portData.at( i );
    }
    portData.clear();

    for( size_t i = 0; i < input.portData.size(); ++i )
    {
        portData.push_back( new DataValuePair( *( input.portData.at( i ) ) ) );
    }

    portNumber = input.portNumber;
    modelName = input.modelName;
    dataFlow = input.dataFlow;
    portLocation = new Point( *( input.portLocation ) );
}
/////////////////////////////////////////////////////
Port& Port::operator=( const Port& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        for( size_t i = 0; i < portData.size(); ++i )
        {
            delete portData.at( i );
        }
        portData.clear();

        for( size_t i = 0; i < input.portData.size(); ++i )
        {
            portData.push_back( new DataValuePair( *( input.portData.at( i ) ) ) );
        }

        portNumber = input.portNumber;
        modelName = input.modelName;
        dataFlow = input.dataFlow;
        portType = input.portType;
        *portLocation = *( input.portLocation );
    }
    return *this;
}
////////////////////////////////////////////
void Port::SetPortNumber( unsigned int number )
{
    portNumber = number;
}
/////////////////////////////////////////////////////
void Port::SetModelName( std::string name )
{
    modelName = name;
}
///////////////////////////////////////////////
void Port::SetDataFlowDirection( std::string direction )
{
    dataFlow = direction;
}
////////////////////////////////////////////////////////////////////////
void Port::SetPortLocation( Point* location )
{
    // we can do this because a point is always created be default
    delete portLocation;
    portLocation = location;
}
///////////////////////////////////////////////////////////////////
void Port::SetPortData( std::vector< DataValuePair* > data )
{
    portData = data;
}
/////////////////////////////////////////////////////
void Port::SetPortType( std::string porttype )
{
    portType = porttype;
}
///////////////////////////////////////
void Port::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetSubElement( "number", portNumber );
    SetSubElement( "name", modelName );
    SetSubElement( "dataFlow", dataFlow );
    SetSubElement( "portLocation", portLocation );
    SetSubElement( "portType", portType );
    for( size_t i = 0; i < portData.size(); ++i )
    {
        SetSubElement( "portData", portData.at( i ) );
    }
}
///////////////////////////////////////////////////
unsigned int Port::GetPortNumber( void )
{
    return portNumber;
}
//////////////////////////////////////////
std::string Port::GetModelName( void )
{
    return modelName;
}
//////////////////////////////////////////
std::string Port::GetDataFlowDirection( void )
{
    return dataFlow;
}
////////////////////////////////////////////
Point* Port::GetPortLocation( void )
{
    return portLocation;
}
//////////////////////////////////////////
std::string Port::GetPortType( void )
{
    return portType;
}
/////////////////////////////////////
std::vector< ves::open::xml::DataValuePair* > Port::GetPortData( void )
{
    return portData;
}
////////////////////////////////////////////////////////////
void Port::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( element );
    }

    if( currentElement )
    {
        //get variables by tags
        DOMElement* dataValueStringName = 0;
        // for port number
        {
            dataValueStringName = GetSubElement( currentElement, "number", 0 );
            GetAttribute( dataValueStringName, "number", portNumber );
        }
        // for model name
        {
            dataValueStringName = GetSubElement( currentElement, "name", 0 );
            GetAttribute( dataValueStringName, "name", modelName );
        }
        // for data flow
        {
            dataValueStringName = GetSubElement( currentElement, "dataFlow", 0 );
            GetAttribute( dataValueStringName, "dataFlow", dataFlow );
        }
        // for port location
        {
            dataValueStringName = GetSubElement( currentElement, "portLocation", 0 );
            if( portLocation )
            {
                delete portLocation;
                portLocation = 0;
            }
            portLocation = new Point( );
            portLocation->SetObjectFromXMLData( dataValueStringName );
        }
        // for port data
        {
            unsigned int numberOfPortData = currentElement->getElementsByTagName(               Convert( "portData" ).toXMLString() )->getLength();

            for( unsigned int i = 0; i < numberOfPortData; ++i )
            {
                dataValueStringName = GetSubElement( currentElement, "portData", i );
                portData.push_back( new DataValuePair( ) );
                portData.back()->SetObjectFromXMLData( dataValueStringName );
            }
        }
    }
}


