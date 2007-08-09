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
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/StateInfo.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_XML;
////////////////////////////////////////////////////////////////////////////////
//Constructors//
////////////////////////////////////////////////////////////////////////////////
User::User()
:XMLObject()
{
   _userId = std::string("User_0");
   _controlStatus = std::string("MASTER");
   SetObjectType("User");
}
////////////////////////////////////////////////////////////////////////////////
User::~User()
{
}
////////////////////////////////////////////////////////////////////////////////
User::User( const User& input )
:XMLObject(input)
{
   _userId = input._userId;
   _controlStatus = input._controlStatus;
   
   if( input.m_stateInfo )
   {
      m_stateInfo = new StateInfo( *(input.m_stateInfo) );
   }
}
////////////////////////////////////////////////////////////////////////////////
User& User::operator=( const User& input)
{
    if( this != &input )
    {
        XMLObject::operator =(input);
        _userId = input._userId;
        _controlStatus = input._controlStatus;

        if( input.m_stateInfo )
        {
            m_stateInfo = new StateInfo( *(input.m_stateInfo) );
        }
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetUserId(std::string id)
{
   _userId = id;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetControlStatus(VEControlStatus cs)
{
   _controlStatus = cs;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetStateInfo( VE_XML::StateInfoWeakPtr userState )
{
   m_stateInfo = userState;
}
////////////////////////////////////////////////////////////////////////////////
std::string User::GetUserId()
{
   return _userId;
}
////////////////////////////////////////////////////////////////////////////////
User::VEControlStatus User::GetControlStatus()
{
   return _controlStatus;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::StateInfoWeakPtr User::GetUserStateInfo()
{
   return m_stateInfo;
}
////////////////////////////////////////////////////////////////////////////////
void User::SetObjectFromXMLData(DOMNode* xmlInput)
{
    DOMElement* currentElement = 0;
    if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
    {
        currentElement = dynamic_cast<DOMElement*>(xmlInput);
    }
   
    if( !currentElement )
    {
        return;
    }

    GetAttribute( currentElement, "id", uuid );
    GetAttribute( currentElement, "userID", _userId );
    GetAttribute( currentElement, "veControlStatus", _controlStatus );

    DOMElement* stateInfoElement = 0;
    stateInfoElement = GetSubElement( currentElement, "stateInfo", 0 );
    if( stateInfoElement )
    {
        m_stateInfo = new StateInfo();
        m_stateInfo->SetObjectFromXMLData( stateInfoElement );
    }
}
////////////////////////////////////////////////////////////////////////////////
void User::_updateVEElement( std::string input )
{
   SetAttribute( "userID", _userId );
   SetAttribute( "id", uuid );
   SetAttribute( "veControlStatus", _controlStatus );
   if( m_stateInfo )
   {
       SetSubElement( "stateInfo", &(*m_stateInfo) );
   }
}

