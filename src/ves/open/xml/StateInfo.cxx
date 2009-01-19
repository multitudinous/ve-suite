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

#include <ves/open/xml/StateInfo.h>
#include <ves/open/xml/Command.h>
#include <algorithm>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;

//////////////////////////
StateInfo::StateInfo()
        : XMLObject()
{
    SetObjectType( "StateInfo" );
}
///////////////////////////
StateInfo::~StateInfo()
{
    ;
}
////////////////////////////////////////////////////
void StateInfo::AddState( CommandPtr state )
{
    mStateInfo.push_back( state );
}
//////////////////////////////
void StateInfo::ClearState()
{
    mStateInfo.clear();
}
////////////////////////////////////////////////////////////////////////////////
void StateInfo::_updateVEElement( const std::string& input )
{
    //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
    //this will be based on the number of commands stored in the state

    //Add code here to update the specific sub elements
    _updateCommands();
}
////////////////////////////////////////////////////////////////////////////////
void StateInfo::_updateCommands()
{
    for( size_t i = 0; i < mStateInfo.size();  i++ )
    {
        mStateInfo.at( i )->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mStateInfo.at( i )->GetXMLData( "Command" ) );
    }
}
/////////////////////////////////////////////////////////////
//set the data from an string representing the xml         //
/////////////////////////////////////////////////////////////
void StateInfo::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    //get variables by tags
    DOMNodeList* subElements = currentElement->getElementsByTagName( ves::open::xml::Convert( "Command" ).toXMLString() );

    mStateInfo.clear();
    //we can have as many dvpairs as we want so get them all and populate the list
    DOMElement* cmdsIn = 0;
    unsigned int nCmdsIn = subElements->getLength();

    //read in new commands
    for( unsigned int i = 0; i < nCmdsIn; i++ )
    {
        DOMElement* vecmdIn = dynamic_cast<DOMElement*>( subElements->item( i ) );
        if( vecmdIn )
        {
            CommandPtr command( new Command() );
            command->SetObjectFromXMLData( vecmdIn );
            mStateInfo.push_back( command );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
CommandPtr StateInfo::GetState( const std::string& name )
{
    for( size_t i = 0; i < mStateInfo.size(); i++ )
    {
        if( mStateInfo.at( i )->GetCommandName() == name )
        {
            return mStateInfo.at( i );
        }
    }
    return CommandPtr();
}
////////////////////////////////////////////////////////////////////////////////
CommandPtr StateInfo::GetState( size_t index )
{
    return mStateInfo.at( index );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< CommandPtr > StateInfo::GetStateVector( void )
{
    std::vector< CommandPtr > tempVec;
    std::copy( mStateInfo.begin(), mStateInfo.end(),
              std::back_inserter( tempVec ) );
    return tempVec;
}
/////////////////////////////////////////////////////
StateInfo::StateInfo( const StateInfo& input )
        : XMLObject( input )
{
    for( size_t i = 0; i < input.mStateInfo.size(); i++ )
    {
        mStateInfo.push_back( CommandPtr( new Command( *( input.mStateInfo.at( i ) ) ) ) );
    }
}
/////////////////////////////////////////////////////////
StateInfo& StateInfo::operator= ( const StateInfo& input )
{
    if( this != &input )
    {
        ClearState();
        for( size_t i = 0; i < input.mStateInfo.size(); i++ )
        {
            mStateInfo.push_back( CommandPtr( new Command( *( input.mStateInfo.at( i ) ) ) ) );
        }
    }
    return *this;
}
