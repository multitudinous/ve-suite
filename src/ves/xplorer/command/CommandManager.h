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
#pragma once

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>
#include <ves/open/xml/CommandPtr.h>

//do this to remove compile warning on linux platforms
#undef _REENTRANT

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>
#include <vpr/Sync/Mutex.h>

// --- C/C++ Includes --- //
#include <vector>

namespace ves
{
namespace xplorer
{
namespace command
{
/*!\file CommandManager.h
 * CommandManager API
 */

/*!\class ves::xplorer::command::CommandManager
 *
 */
class VE_XPLORER_COMMAND_EXPORTS CommandManager
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< CommandManager >;
    //CommandManager(const CommandManager& o) { ; }
    //CommandManager& operator=(const CommandManager& o) { ; }

    ///Constructor
    CommandManager();

    ///Destructor
    ~CommandManager();
    vprSingletonHeader( CommandManager );

public:
    ///Initialize environment.
    void Initialize();

    ///Add XML command to command queue
    void AddXMLCommand( const ves::open::xml::CommandPtr& commandIn );

    ///Get XML command from command queue
    const ves::open::xml::CommandPtr& GetXMLCommand();
    
    ///Get all of the XML commands that have this command name
    ///\param commandName The command you are after
    std::vector< ves::open::xml::CommandPtr > 
        GetXMLCommands( std::string const& commandName );

    ///Initialize scene.
    void InitScene();

    ///Pre frame update.
    void PreFrameUpdate();

    ///Late pre-frame update
    void LatePreFrameUpdate();

private:
    ///Command vector queue
    std::vector< ves::open::xml::CommandPtr > m_commandVectorQueue;
    ///A mutex to protect command queue accesses
    vpr::Mutex m_valueLock;
    ///Null Command Ptr
    ves::open::xml::CommandPtr m_nullCommand;
    ///Active Command Ptr
    ves::open::xml::CommandPtr m_activeCommand;
};
}
}    
}
