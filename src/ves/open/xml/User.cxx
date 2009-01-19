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

#include <ves/open/xml/User.h>
#include <ves/open/xml/StateInfo.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
////////////////////////////////////////////////////////////////////////////////
//Constructors//
////////////////////////////////////////////////////////////////////////////////
User::User()
        : XMLObject()
{
    mUserId = std::string( "User_0" );
    mControlStatus = std::string( "MASTER" );
    SetObjectType( "User" );
}
////////////////////////////////////////////////////////////////////////////////
User::~User()
{}
////////////////////////////////////////////////////////////////////////////////
User::User( const User& input )
        : XMLObject( input )
{
    mUserId = input.mUserId;
    mControlStatus = input.mControlStatus;

    if( input.mStateInfo )
    {
        mStateInfo = StateInfoPtr( new StateInfo( *(  input.mStateInfo ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
User& User::operator=( const User& input )
{
    if( this != &input )
    {
        XMLObject::operator =( input );
        mUserId = input.mUserId;
        mControlStatus = input.mControlStatus;

        if( input.mStateInfo )
        {
            mStateInfo = StateInfoPtr( new StateInfo( *(  input.mStateInfo ) ) );
        }
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetUserId( const std::string& id )
{
    mUserId = id;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetControlStatus( VEControlStatus cs )
{
    mControlStatus = cs;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetStateInfo( StateInfoPtr userState )
{
    mStateInfo = userState;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& User::GetUserId()
{
    return mUserId;
}
////////////////////////////////////////////////////////////////////////////////
User::VEControlStatus User::GetControlStatus()
{
    return mControlStatus;
}
////////////////////////////////////////////////////////////////////////////////
StateInfoPtr User::GetUserStateInfo()
{
    return mStateInfo;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetObjectFromXMLData( DOMNode* xmlInput )
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

    GetAttribute( currentElement, "id", mUuid );
    GetAttribute( currentElement, "userID", mUserId );
    GetAttribute( currentElement, "veControlStatus", mControlStatus );

    DOMElement* stateInfoElement = 0;
    stateInfoElement = GetSubElement( currentElement, "stateInfo", 0 );
    if( stateInfoElement )
    {
        mStateInfo = StateInfoPtr( new StateInfo() );
        mStateInfo->SetObjectFromXMLData( stateInfoElement );
    }
}
////////////////////////////////////////////////////////////////////////////////
void User::_updateVEElement( const std::string& input )
{
    SetAttribute( "userID", mUserId );
    SetAttribute( "id", mUuid );
    SetAttribute( "veControlStatus", mControlStatus );
    if( mStateInfo )
    {
        SetSubElement<ves::open::xml::XMLObjectPtr>( "stateInfo", mStateInfo );
    }
}

