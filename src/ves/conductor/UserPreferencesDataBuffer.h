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
#ifndef USER_PREFERENCES_DATA_BUFFER_H
#define USER_PREFERENCES_DATA_BUFFER_H
/*!\file UserPreferencesDataBuffer.h
UserPreferencesDataBuffer API
*/
/*!\class VE_Conductor::UserPreferencesDataBuffer
*
*/
#include <ves/open/xml/CommandPtr.h>

//do this to remove compile warning from linux platforms
#undef _REENTRANT
#include <vpr/Util/Singleton.h>
#include <vpr/Sync/Mutex.h>

#include <map>
#include <string>

#include <ves/VEConfig.h>

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS UserPreferencesDataBuffer
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    // friend class vpr::Singleton< UserPreferenceDataBuffer >;
    UserPreferencesDataBuffer( void );
    ~UserPreferencesDataBuffer();
    vprSingletonHeader( UserPreferencesDataBuffer );
public:
    ///Destructor
    //~CORBAServiceList( void );
    void CleanUp( void );
    ///Get Command with key
    ///The key MUST be the command name
    const ves::open::xml::CommandPtr& GetCommand( const std::string& commandKey );
    ///set Command with key
    void SetCommand( const std::string& commandKey, const ves::open::xml::CommandPtr& command );
    ///Get all the commands
    std::map< std::string, ves::open::xml::CommandWeakPtr > GetCommandMap( void );
    ///Set all the commands
    void SetCommandMap( const std::map< std::string, ves::open::xml::CommandWeakPtr >& tempMap );
private:
    ///Mapp to hold all the preference data to be written to the ves file
    std::map< std::string, ves::open::xml::CommandPtr > commandMap;
    //std::map< std::string, ves::open::xml::CommandPtr > commandMap;
    ///A mutex to protect variables accesses
    vpr::Mutex m_valueLock;
};
}
}
#endif
