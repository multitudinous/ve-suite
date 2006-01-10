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
 * File:          $RCSfile: VEUser.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/VEUser.h"
#include "VE_Open/VE_XML/VEStateInfo.h"

using namespace VE_XML;
////////////////
//Constructors//
////////////////
VEUser::VEUser(DOMDocument* rootDoc)
:VEXMLObject(rootDoc)
{
   _stateInfo = 0;
   _userId = std::string("VEUser_0");
   _controlStatus = std::string("MASTER");

}
/////////////////
VEUser::~VEUser()
{
   
}
//////////////////////////////////////
void VEUser::SetUserId(std::string id)
{
   _userId = id;
}
/////////////////////////////////////////////////////////
void VEUser::SetControlStatus(VEControlStatus cs)
{
   _controlStatus = cs;
}
////////////////////////////////////////////////////////
void VEUser::SetStateInfo(VE_XML::VEStateInfo* userState)
{
   _stateInfo = userState;
}
///////////////////////////////
std::string VEUser::GetUserId()
{
   return _userId;
}
//////////////////////////////////////////////////
VEUser::VEControlStatus VEUser::GetControlStatus()
{
   return _controlStatus;
}
///////////////////////////////////////////////
VE_XML::VEStateInfo* VEUser::GetUserStateInfo()
{
   return _stateInfo;
}
////////////////////////////////////////////////////////
void VEUser::SetObjectFromXMLData(DOMNode* xmlInput)
{
}
//////////////////////////////
void VEUser::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString( input ));
      
   }
   //Be sure to set the number of children either here or in the updating subElements code
   //we know this to be 3: 
   //stateInfo element;
   //userId element;
   //controlStatus element;
   _nChildren = 3;

   //Add code here to update the specific sub elements

}

