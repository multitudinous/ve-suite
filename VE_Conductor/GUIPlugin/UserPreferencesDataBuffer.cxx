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
#include "VE_Open/XML/XMLReaderWriter.h"

#include <sstream>

#include <wx/wx.h>
#include <wx/app.h>
#include <wx/utils.h>

using namespace VE_XML;
using namespace VE_Conductor;

vprSingletonImp( UserPreferencesDataBuffer );

////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::CleanUp( void )
{
   ;
}
///Get Command with key
///The key MUST be the command name
VE_XML::Command UserPreferencesDataBuffer::GetCommand( std::string commandKey )
{
   ;
}
///Get Command with key
void UserPreferencesDataBuffer::SetCommand( std::string commandKey, VE_XML::Command command )
{
   ;
}
///Get all the commands
std::map< std::string, VE_XML::Command > UserPreferencesDataBuffer::GetCommandMap( void )
{
   std::map< std::string, VE_XML::Command > temp;
   return temp;
}
///Set all the commands
void UserPreferencesDataBuffer::SetCommandMap( std::map< std::string, VE_XML::Command > )
{
   ;
}
