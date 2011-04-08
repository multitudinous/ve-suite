/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/Command.h>
//#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>

vprSingletonImpLifetime( ves::xplorer::command::CommandManager, 1 );

namespace ves
{
namespace xplorer
{
namespace command
{
////////////////////////////////////////////////////////////////////////////////
CommandManager::CommandManager()
{
}
////////////////////////////////////////////////////////////////////////////////
CommandManager::~CommandManager()
{
}
////////////////////////////////////////////////////////////////////////////////
void CommandManager::InitScene()
{
}
////////////////////////////////////////////////////////////////////////////////
void CommandManager::PreFrameUpdate()
{
}
////////////////////////////////////////////////////////////////////////////////
void CommandManager::LatePreFrameUpdate()
{
    vprDEBUG( vesDBG, 3 ) << "|\tCommandManager::LatePreFrameUpdate " 
        << std::endl << vprDEBUG_FLUSH;
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    if( m_commandVectorQueue.empty() )
    {
        m_activeCommand = m_nullCommand;
        return;
    }
    
    std::vector< ves::open::xml::CommandPtr >::iterator iter;
    iter = m_commandVectorQueue.begin();
    m_activeCommand = ves::open::xml::CommandPtr( new ves::open::xml::Command( *( *iter ) ) );
    m_commandVectorQueue.erase( iter );
    vprDEBUG( vesDBG, 3 ) << "|\tEnd CommandManager::LatePreFrameUpdate " 
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void CommandManager::AddXMLCommand( const ves::open::xml::CommandPtr& commandIn )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    m_commandVectorQueue.push_back( commandIn );
}
////////////////////////////////////////////////////////////////////////////////
const ves::open::xml::CommandPtr& CommandManager::GetXMLCommand()
{    
    return m_activeCommand;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< ves::open::xml::CommandPtr > 
    CommandManager::GetXMLCommands( std::string const& commandName )
{    
    std::vector< ves::open::xml::CommandPtr > vectorQueue;
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    for( std::vector< ves::open::xml::CommandPtr >::iterator 
        iter = m_commandVectorQueue.begin(); 
        iter != m_commandVectorQueue.end(); )
    {
        if( (*iter)->GetCommandName() == commandName )
        {
            vectorQueue.push_back( ves::open::xml::CommandPtr( new ves::open::xml::Command( *( *iter ) ) ) );
            iter = m_commandVectorQueue.erase( iter );
        }
        else
        {
            iter++;
        }
    }
    return vectorQueue;
}
////////////////////////////////////////////////////////////////////////////////
}
} // end xplorer
} // end ves
