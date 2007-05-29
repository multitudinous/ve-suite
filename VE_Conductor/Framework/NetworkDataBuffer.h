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
#ifndef NETWORK_DATA_BUFFER_H
#define NETWORK_DATA_BUFFER_H
/*!\file NetworkDatabuffer.h
NetworkDatabuffer API
*/
/*!\class VE_Conductor::NetworkDatabuffer
* 
*/
#include "VE_Open/XML/Command.h"

#include "VE_Conductor/Utilities/Link.h"
#include "VE_Conductor/GUIPlugin/Module.h"
#include "VE_Conductor/Utilities/Tag.h"
#include "VE_Installer/include/VEConfig.h"
//do this to remove compile warnings from linux platforms
#undef _REENTRANT
#include <vpr/Util/Singleton.h>
#include <vpr/Sync/Mutex.h>

#include <map>
#include <string>


namespace VE_Conductor
{
class NetworkDatabuffer
{
public:
    ///Constructor
    NetworkDatabuffer( void );
    ///Destructor
    ~NetworkDatabuffer();
    ///Get Command with key
    ///The key MUST be the command name
    ///\param commandKey command desired by the user
    ///\return Return command selected byt the user
    VE_XML::Command& GetCommand( std::string commandKey );
    ///set Command with key
    ///\param commandKey key of the command desired
    ///\param command command to be stored
    void SetCommand( std::string commandKey, VE_XML::Command& command );
    ///Get all the commands
    ///\return Returns the m_commandMap
    std::map< std::string, VE_XML::Command >& GetCommandMap( void );
    ///Set all the commands
    ///\param tempMap the the map of commands when initialized by the user
    void SetCommandMap( std::map< std::string, VE_XML::Command >& tempMap );
    ///Set data from CORBA receiver thread
    void SetXplorerData(){ ; }
    ///Load data 
    void LoadVESData( std::string vesNetwork );
    ///Save data 
    std::string SaveVESData( std::string fileName );
    ///New 
    void NewVESData( bool promptClearXplorer );
    ///Set Window
    void SetRenderWindow( wxWindow* renderWindow );
    
private:
    ///Map to store the command name and command for easy lookup by the user
    std::map< std::string, VE_XML::Command > m_commandMap;
    ///mutex to lock the command map so that it is accessed appropriately
    vpr::Mutex m_commandMapLock;
    //Three main list of network objs
    std::vector< VE_Conductor::GUI_Utilities::Link > links; //The list of links between the nodes of moduls.
    std::vector< VE_Conductor::GUI_Utilities::Tag > tags; //The list of text tags  
    std::map< int, VE_Conductor::GUI_Utilities::Module > modules; //The list of modules;
    wxWindow* renderWindow;
};
}
#endif //NETWORK_DATA_BUFFER_H
