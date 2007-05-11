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
 * Date modified: $Date: 2007-02-27 22:54:03 -0600 (Tue, 27 Feb 2007) $
 * Version:       $Rev: 7013 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"

#include <sstream>

using namespace VE_XML;
using namespace VE_Conductor;

vprSingletonImp( UserPreferencesDataBuffer );
////////////////////////////////////////////////////////////////////////////////
UserPreferencesDataBuffer::UserPreferencesDataBuffer( void )
{ 
   VE_XML::Command nullCommand;
   nullCommand.SetCommandName( "NULL" );
   commandMap[ "NULL" ] = nullCommand;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::CleanUp( void )
{
   commandMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command& UserPreferencesDataBuffer::GetCommand( std::string commandKey )
{
   std::map< std::string, VE_XML::Command >::iterator iter;
   iter = commandMap.find( commandKey );
   if ( iter == commandMap.end() )
   {
      return commandMap[ "NULL" ];
   }
   return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::SetCommand( std::string commandKey, VE_XML::Command& command )
{
   commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::Command >& UserPreferencesDataBuffer::GetCommandMap( void )
{
   return commandMap;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::SetCommandMap( std::map< std::string, VE_XML::Command >& tempMap )
{
   commandMap = tempMap;
}
////////////////////////////////////////////////////////////////////////////////
