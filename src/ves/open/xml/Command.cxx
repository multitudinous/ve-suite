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
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
XERCES_CPP_NAMESPACE_USE
#include <iostream>
using namespace ves::open::xml;
//////////////////////
//Constructor       //
//////////////////////
Command::Command()
        : XMLObject()
{
    _cmdName.empty();
    _dataValuePairs.clear();
    SetObjectType( "Command" );
}
///////////////////////
Command::~Command()
{
    _dataValuePairs.clear();
    nameToDataValuePairMap.clear();
}
///////////////////////////////////////////
Command::Command( const Command& input )
        : XMLObject( input )
{
    _cmdName =  input._cmdName;
    for( size_t i = 0; i < input._dataValuePairs.size(); ++i )
    {
        _dataValuePairs.push_back( new DataValuePair(( *( input._dataValuePairs.at( i ) ) ) ) );
        nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName()] = _dataValuePairs.back();
    }
}
/////////////////////////////////////////////////////
Command& Command::operator=( const Command& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        _cmdName =  input._cmdName;

        _dataValuePairs.clear();
        nameToDataValuePairMap.clear();

        for( size_t i = 0; i < input._dataValuePairs.size(); ++i )
        {
            _dataValuePairs.push_back( new DataValuePair(( *( input._dataValuePairs.at( i ) ) ) ) );
            nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName()] = _dataValuePairs.back();
        }
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
/*void Command::AddDataValuePair(DataValuePair* commandValuePair)
{
   _dataValuePairs.push_back(commandValuePair);
   nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName() ] = commandValuePair;
}*/
////////////////////////////////////////////////////////////////////////////////
void Command::AddDataValuePair( DataValuePairWeakPtr commandValuePair )
{
    _dataValuePairs.push_back( commandValuePair );
    nameToDataValuePairMap[ _dataValuePairs.back()->GetDataName()] = commandValuePair;
}
////////////////////////////////////////////////////////////////////////////////
void Command::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
    //_nChildren will be the number of dvPairs + the name of the command but we have to call the
    //update functions below to get the ndvPairs before we can calculate _nChildren

    //Add code here to update the specific sub elements
    SetAttribute( "commandName", _cmdName );
    _updateDataValuePairs();
}
////////////////////////////////////
void Command::_updateCommandName()
{
    DOMElement* cmdNameElement = mRootDocument->createElement(
                                 Convert( "command" ).toXMLString() );

    DOMText* cmdName = mRootDocument->createTextNode(
                       Convert( _cmdName ).toXMLString() );

    cmdNameElement->appendChild( cmdName );
    mVeElement->appendChild( cmdNameElement );
}
///////////////////////////////////////
void Command::_updateDataValuePairs()
{
    for( size_t i = 0; i < _dataValuePairs.size();  ++i )
    {
        _dataValuePairs.at( i )->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( _dataValuePairs.at( i )->GetXMLData( "parameter" ) );
    }
}
/////////////////////////////////////////////////////////
//set the data from an string representing the xml     //
/////////////////////////////////////////////////////////
void Command::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( !currentElement )
    {
        return;
    }
    //break down the element
    {
        //get variables by tags
        DOMNodeList* subElements = 0;
        subElements = currentElement->getElementsByTagName(
                      Convert( "command" ).toXMLString() );

        if( subElements->getLength() > 0 )
        {
            //should only be the name of the command
            DOMElement* name = dynamic_cast< DOMElement* >( subElements->item( 0 ) );
            GetAttribute( name, "commandName", _cmdName );
        }
        else
        {
            GetAttribute( currentElement, "commandName", _cmdName );
        }
    }
    //break down the element
    {
        //get variables by tags
        DOMNodeList* subElements = currentElement->getElementsByTagName(
                                   Convert( "parameter" ).toXMLString() );

        //clear out old dvpairs
        _dataValuePairs.clear();
        nameToDataValuePairMap.clear();
        //we can have as many dvpairs as we want so get them all and populate the list
        DOMElement* dataValuePairIn = 0;
        unsigned int nDVPairsIn = subElements->getLength();
        //read in new data value pairs
        for( unsigned int i = 0; i < nDVPairsIn; ++i )
        {
            DOMElement* dvPairIn = dynamic_cast<DOMElement*>( subElements->item( i ) );
            DataValuePairPtr veDvp = new DataValuePair();
            veDvp->SetObjectFromXMLData( dvPairIn );
            _dataValuePairs.push_back( veDvp );
            nameToDataValuePairMap[ veDvp->GetDataName()] = veDvp;
        }
    }
}
///////////////////////////////////////
const std::string Command::GetCommandName()
{
    return _cmdName;
}
///////////////////////////////////////
void Command::SetCommandName( std::string name )
{
    _cmdName = name;
}
//////////////////////////////////////////////////////////////////////////////
DataValuePairWeakPtr Command::GetDataValuePair( std::string dataValueName )
{
    std::map< std::string, DataValuePairPtr >::iterator iter;
    iter = nameToDataValuePairMap.find( dataValueName );
    if( iter != nameToDataValuePairMap.end() )
    {
        return iter->second;
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////
DataValuePairWeakPtr Command::GetDataValuePair( size_t index )
{
    try
    {
        return _dataValuePairs.at( index );
    }
    catch ( ... )
    {
        std::cerr << " Command::GetDataValuePair The element request "
        << "is out of sequence. Please ask for a lower number point."
        << std::endl;

        return 0;
    }
}
///////////////////////////////////////////////////
size_t Command::GetNumberOfDataValuePairs()
{
    return _dataValuePairs.size();
}

