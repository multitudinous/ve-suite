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
    mPortNumber = 0;
    //mPluginName = '\0';
    //mDataFlow = '\0';
    mPortLocation = PointPtr( new Point() );
    SetObjectType( "Port" );
    SetObjectNamespace( "Model" );
}
///////////////////////////////////
Port::~Port()
{
    mPortData.clear();
}
///////////////////////////////////////////
Port::Port( const Port& input )
        : XMLObject( input )
{
    mPortData.clear();

    for( size_t i = 0; i < input.mPortData.size(); ++i )
    {
        mPortData.push_back( DataValuePairPtr( new DataValuePair( *( input.mPortData.at( i ) ) ) ) );
    }

    mPortNumber = input.mPortNumber;
    mPluginName = input.mPluginName;
    mDataFlow = input.mDataFlow;
    mPortLocation = PointPtr( new Point( *( input.mPortLocation ) ) );
}
/////////////////////////////////////////////////////
Port& Port::operator=( const Port& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mPortData.clear();

        for( size_t i = 0; i < input.mPortData.size(); ++i )
        {
            mPortData.push_back( DataValuePairPtr( new DataValuePair( *( input.mPortData.at( i ) ) ) ) );
        }

        mPortNumber = input.mPortNumber;
        mPluginName = input.mPluginName;
        mDataFlow = input.mDataFlow;
        mPortType = input.mPortType;
        *mPortLocation = *( input.mPortLocation );
    }
    return *this;
}
////////////////////////////////////////////
void Port::SetPortNumber( unsigned int number )
{
    mPortNumber = number;
}
/////////////////////////////////////////////////////
void Port::SetPluginName( const std::string& name )
{
    mPluginName = name;
}
///////////////////////////////////////////////
void Port::SetDataFlowDirection( const std::string& direction )
{
    mDataFlow = direction;
}
////////////////////////////////////////////////////////////////////////
void Port::SetPortLocation( PointPtr location )
{
    // we can do this because a point is always created be default
    mPortLocation = location;
}
///////////////////////////////////////////////////////////////////
void Port::SetPortData( const std::vector< DataValuePairPtr >& data )
{
    mPortData = data;
}
/////////////////////////////////////////////////////
void Port::SetPortType( const std::string& portType )
{
    mPortType = portType;
}
///////////////////////////////////////
void Port::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetSubElement( "number", mPortNumber );
    SetSubElement( "name", mPluginName );
    SetSubElement( "dataFlow", mDataFlow );
    SetSubElement<ves::open::xml::XMLObjectPtr>( "portLocation", mPortLocation );
    SetSubElement( "portType", mPortType );
    SetSubElements( "portData", mPortData );
}
///////////////////////////////////////////////////
unsigned int Port::GetPortNumber( void )
{
    return mPortNumber;
}
//////////////////////////////////////////
const std::string& Port::GetPluginName( void )
{
    return mPluginName;
}
//////////////////////////////////////////
const std::string& Port::GetDataFlowDirection( void )
{
    return mDataFlow;
}
////////////////////////////////////////////
PointPtr Port::GetPortLocation( void )
{
    return mPortLocation;
}
//////////////////////////////////////////
const std::string& Port::GetPortType( void )
{
    return mPortType;
}
/////////////////////////////////////
const std::vector< ves::open::xml::DataValuePairPtr >& Port::GetPortData( void )
{
    return mPortData;
}
////////////////////////////////////////////////////////////
void Port::SetObjectFromXMLData( DOMNode* element )
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
    // for port number
    {
        dataValueStringName = GetSubElement( currentElement, "number", 0 );
        GetDataFromElement( dataValueStringName, mPortNumber );
    }
    // for model name
    {
        dataValueStringName = GetSubElement( currentElement, "name", 0 );
        GetDataFromElement( dataValueStringName, mPluginName );
    }
    // for data flow
    {
        dataValueStringName = GetSubElement( currentElement, "dataFlow", 0 );
        GetDataFromElement( dataValueStringName, mDataFlow );
    }
    // for port location
    {
        dataValueStringName = GetSubElement( currentElement, "portLocation", 0 );
        mPortLocation = PointPtr( new Point() );
        mPortLocation->SetObjectFromXMLData( dataValueStringName );
    }
    // for port data
    {
        unsigned int numberOfmPortData = currentElement->getElementsByTagName(               
            Convert( "portData" ).toXMLString() )->getLength();

        for( unsigned int i = 0; i < numberOfmPortData; ++i )
        {
            dataValueStringName = GetSubElement( currentElement, "portData", i );
            mPortData.push_back( DataValuePairPtr( new DataValuePair( ) ) );
            mPortData.back()->SetObjectFromXMLData( dataValueStringName );
        }
    }
}


