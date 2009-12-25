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
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/lambda/lambda.hpp>
#include <boost/concept_check.hpp>

XERCES_CPP_NAMESPACE_USE
#include <iostream>
using namespace ves::open::xml;
//////////////////////
//Constructor       //
//////////////////////
Command::Command()
        : XMLObject()
{
    mCmdName.empty();
    mDataValuePairs.clear();
    SetObjectType( "Command" );
}
///////////////////////
Command::~Command()
{
    mDataValuePairs.clear();
    mNameToDataValuePairMap.clear();
}
///////////////////////////////////////////
Command::Command( const Command& input )
        : XMLObject( input )
{
    mCmdName =  input.mCmdName;
    for( size_t i = 0; i < input.mDataValuePairs.size(); ++i )
    {
        mDataValuePairs.push_back( DataValuePairPtr( new DataValuePair(( *( input.mDataValuePairs.at( i ) ) ) ) ) );
        mNameToDataValuePairMap[ mDataValuePairs.back()->GetDataName()] = mDataValuePairs.back();
    }
}
/////////////////////////////////////////////////////
Command& Command::operator=( const Command& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mCmdName =  input.mCmdName;

        mDataValuePairs.clear();
        mNameToDataValuePairMap.clear();

        for( size_t i = 0; i < input.mDataValuePairs.size(); ++i )
        {
            mDataValuePairs.push_back( DataValuePairPtr( new DataValuePair(( *( input.mDataValuePairs.at( i ) ) ) ) ) );
            mNameToDataValuePairMap[ mDataValuePairs.back()->GetDataName()] = mDataValuePairs.back();
        }
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
/*void Command::AddDataValuePair(DataValuePair* commandValuePair)
{
   mDataValuePairs.push_back(commandValuePair);
   mNameToDataValuePairMap[ mDataValuePairs.back()->GetDataName() ] = commandValuePair;
}*/
////////////////////////////////////////////////////////////////////////////////
void Command::AddDataValuePair( DataValuePairPtr commandValuePair )
{

    if( mNameToDataValuePairMap.find( commandValuePair->GetDataName() ) == mNameToDataValuePairMap.end() )
    {
        mDataValuePairs.push_back( commandValuePair );
        mNameToDataValuePairMap[ commandValuePair->GetDataName() ] = commandValuePair;
        return;
    }

    mNameToDataValuePairMap[ commandValuePair->GetDataName() ] = commandValuePair;

    bool hasDVP = false;
    for( std::vector< DataValuePairPtr >::iterator iter = mDataValuePairs.begin(); iter != mDataValuePairs.end(); ++iter )
    {
        if( (*iter)->GetDataName() == commandValuePair->GetDataName() )
        {
            hasDVP = true;
            break;
        }
    }
    
    if( hasDVP )
    {
        mDataValuePairs.push_back( commandValuePair );
    }
 }
////////////////////////////////////////////////////////////////////////////////
void Command::RemoveDataValuePair( const std::string& dataValueName )
{
  std::map< std::string, DataValuePairPtr >::iterator iter ( mNameToDataValuePairMap.find ( dataValueName ) );
  if ( iter != mNameToDataValuePairMap.end() )
  {
    mDataValuePairs.erase ( std::find_if ( mDataValuePairs.begin(), mDataValuePairs.end(), boost::lambda::_1 == iter->second ) );
    mNameToDataValuePairMap.erase ( iter );
  }
}
////////////////////////////////////////////////////////////////////////////////
void Command::_updateVEElement( const std::string& input )
{
    boost::ignore_unused_variable_warning( input );

    //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
    //_nChildren will be the number of dvPairs + the name of the command but we have to call the
    //update functions below to get the ndvPairs before we can calculate _nChildren

    //Add code here to update the specific sub elements
    SetAttribute( "commandName", mCmdName );
    _updateDataValuePairs();
}
////////////////////////////////////
void Command::_updateCommandName()
{
    DOMElement* cmdNameElement = mRootDocument->createElement(
                                 Convert( "command" ).toXMLString() );

    DOMText* cmdName = mRootDocument->createTextNode(
                       Convert( mCmdName ).toXMLString() );

    cmdNameElement->appendChild( cmdName );
    mVeElement->appendChild( cmdNameElement );
}
///////////////////////////////////////
void Command::_updateDataValuePairs()
{
    for( size_t i = 0; i < mDataValuePairs.size();  ++i )
    {
        mDataValuePairs.at( i )->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mDataValuePairs.at( i )->GetXMLData( "parameter" ) );
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
            DOMElement* name = static_cast< DOMElement* >( subElements->item( 0 ) );
            GetDataFromElement( name, mCmdName );
        }
        else
        {
            GetAttribute( currentElement, "commandName", mCmdName );
        }
    }
    //break down the element
    {
        //get variables by tags
        DOMNodeList* subElements = currentElement->getElementsByTagName(
                                   Convert( "parameter" ).toXMLString() );

        //clear out old dvpairs
        mDataValuePairs.clear();
        mNameToDataValuePairMap.clear();
        //we can have as many dvpairs as we want so get them all and populate the list
        DOMElement* dataValuePairIn = 0;
        unsigned int nDVPairsIn = subElements->getLength();
        //read in new data value pairs
        for( unsigned int i = 0; i < nDVPairsIn; ++i )
        {
            dataValuePairIn = static_cast<DOMElement*>( subElements->item( i ) );
            if( dataValuePairIn->getParentNode() == currentElement )
            {
                DataValuePairPtr veDvp( new DataValuePair() );
                veDvp->SetObjectFromXMLData( dataValuePairIn );
                mDataValuePairs.push_back( veDvp );
                mNameToDataValuePairMap[ veDvp->GetDataName()] = veDvp;
            }
            dataValuePairIn = 0;
        }
    }
}
///////////////////////////////////////
const std::string& Command::GetCommandName()
{
    return mCmdName;
}
///////////////////////////////////////
void Command::SetCommandName( const std::string& name )
{
    mCmdName = name;
}
//////////////////////////////////////////////////////////////////////////////
DataValuePairPtr Command::GetDataValuePair( const std::string& dataValueName )
{
    std::map< std::string, DataValuePairPtr >::iterator iter;
    iter = mNameToDataValuePairMap.find( dataValueName );
    if( iter != mNameToDataValuePairMap.end() )
    {
        return iter->second;
    }
    return DataValuePairPtr();
}
////////////////////////////////////////////////////////////////////////
DataValuePairPtr Command::GetDataValuePair( size_t index )
{
    try
    {
        return mDataValuePairs.at( index );
    }
    catch ( ... )
    {
        std::cerr << " Command::GetDataValuePair The element request "
        << "is out of sequence. Please ask for a lower number point."
        << std::endl;

        return DataValuePairPtr();
    }
}
///////////////////////////////////////////////////
size_t Command::GetNumberOfDataValuePairs()
{
    return mDataValuePairs.size();
}

