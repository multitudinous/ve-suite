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
#include <ves/conductor/UserPreferencesDataBuffer.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <sstream>
#include <algorithm>

using namespace ves::open::xml;
using namespace ves::conductor;

vprSingletonImp( UserPreferencesDataBuffer );
////////////////////////////////////////////////////////////////////////////////
UserPreferencesDataBuffer::UserPreferencesDataBuffer( void )
{
    ves::open::xml::CommandPtr nullCommand = new Command();
    nullCommand->SetCommandName( "NULL" );
    commandMap[ "NULL" ] = nullCommand;
}
////////////////////////////////////////////////////////////////////////////////
UserPreferencesDataBuffer::~UserPreferencesDataBuffer()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::CleanUp( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    commandMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
const ves::open::xml::CommandPtr& UserPreferencesDataBuffer::GetCommand(
    const std::string& commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    std::map< std::string, ves::open::xml::CommandPtr >::iterator iter;
    iter = commandMap.find( commandKey );
    if( iter == commandMap.end() )
    {
        return commandMap[ "NULL" ];
    }

    ///check and make sure iter->second is valid first
    if( !iter->second )
    {
        std::cerr << "Preference variable is NULL." << std::endl;
        return commandMap[ "NULL" ];
    }

    return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::SetCommand( const std::string& commandKey,
                                            const ves::open::xml::CommandPtr& command )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, ves::open::xml::CommandWeakPtr > UserPreferencesDataBuffer::GetCommandMap( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    std::map< std::string, ves::open::xml::CommandWeakPtr > tempMap;
    for( std::map< std::string, ves::open::xml::CommandPtr >::iterator
            iter = commandMap.begin(); iter != commandMap.end(); )
    {
        if( iter->first == "NULL" )
        {
            ++iter;
            continue;
        }

        ///check and make sure iter->second is valid first
        if( !iter->second )
        {
            commandMap.erase( iter++ );
        }
        else
        {
            tempMap[ iter->first ] = iter->second;
            ++iter;
        }
    }
    //std::copy( commandMap.begin(), commandMap.end(), tempMap.begin() );
    return tempMap;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferencesDataBuffer::SetCommandMap( const std::map< std::string, ves::open::xml::CommandWeakPtr >& tempMap )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    commandMap.clear();
    for( std::map< std::string, ves::open::xml::CommandWeakPtr >::const_iterator
            iter = tempMap.begin(); iter != tempMap.end(); ++iter )
    {
        commandMap[ iter->first ] = iter->second;
    }
    //std::copy( tempMap.begin(), tempMap.end(), commandMap.begin() );
    std::map< std::string, ves::open::xml::CommandPtr >::iterator iter;
    iter = commandMap.find( "NULL" );
    if( iter == commandMap.end() )
    {
        ves::open::xml::CommandPtr nullCommand = new Command();
        nullCommand->SetCommandName( "NULL" );
        commandMap[ "NULL" ] = nullCommand;
    }
}
////////////////////////////////////////////////////////////////////////////////
