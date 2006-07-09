/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
////////////////
//Constructors//
////////////////
User::User()
:XMLObject()
{
   _stateInfo = 0;
   _userId = std::string("User_0");
   _controlStatus = std::string("MASTER");
   SetObjectType("User");

}
/////////////////
User::~User()
{
   
}
//////////////////////////////////////
void User::SetUserId(std::string id)
{
   _userId = id;
}
/////////////////////////////////////////////////////////
void User::SetControlStatus(VEControlStatus cs)
{
   _controlStatus = cs;
}
////////////////////////////////////////////////////////
void User::SetStateInfo(VE_XML::StateInfo* userState)
{
   _stateInfo = userState;
}
///////////////////////////////
std::string User::GetUserId()
{
   return _userId;
}
//////////////////////////////////////////////////
User::VEControlStatus User::GetControlStatus()
{
   return _controlStatus;
}
///////////////////////////////////////////////
VE_XML::StateInfo* User::GetUserStateInfo()
{
   return _stateInfo;
}
////////////////////////////////////////////////////////
void User::SetObjectFromXMLData(DOMNode* xmlInput)
{
}
//////////////////////////////
void User::_updateVEElement( std::string input )
{
   //Be sure to set the number of children either here or in the updating subElements code
   //we know this to be 3: 
   //stateInfo element;
   //userId element;
   //controlStatus element;
   //_nChildren = 3;

   //Add code here to update the specific sub elements

}

