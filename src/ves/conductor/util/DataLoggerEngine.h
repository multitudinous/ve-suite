/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef VES_CONDUCTOR_UTIL_DATALOGGERENGINE
#define VES_CONDUCTOR_UTIL_DATALOGGERENGINE
/*!\file DataLoggerEngine.h
DataLoggerEngine API
*/
/*!\class ves::conductor::util::DataLoggerEngine
*
*/
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/XMLObjectPtr.h>

//do this to remove compile warning from linux platforms
#undef _REENTRANT
#include <vpr/Util/Singleton.h>

#include <vector>
#include <string>

#include <ves/VEConfig.h>

namespace vpr
{
class Timer;
}

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS DataLoggerEngine
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< SceneManager >;
    DataLoggerEngine( void );
    ///Destructor
    ~DataLoggerEngine();
    vprSingletonHeader( DataLoggerEngine );

public:
    ///Destructor
    void CleanUp( void );
    ///Set a naming context
    ///\param naming_context
    ///Return the list of services that are connected to the name server

    ///Set xplorer command string
    ///\param command string containing command
    bool SendCommandStringToXplorer( const ves::open::xml::CommandWeakPtr& veCommand );
    ///Set ce network string
    ///\param network string containing network
    bool SendNetworkStringToCE( const std::string& network );

    void LoadVEMFile( const std::string& file );
    
    void PlayVEMFile();
    
    void ToggleOn( bool turnOn );
    
    void SetMovieFilename( const std::string& filename );
 
    void LoopingOn( bool looping );
    
    bool IsPlaying();

private:
    void PlayThread();

    void WriteFile();
    
    /// command vector to store animation data
    std::vector< ves::open::xml::CommandPtr > m_dataLoggerCommandVectorQueue;
    std::vector< ves::open::xml::XMLObjectPtr > m_loadedCommands;
    ///Timer to record when commands are stored
    vpr::Timer* m_commandTimer;
    ///Data logging
    bool m_dataLogging;
    ///Movie filename
    std::string m_movieFilename;
    ///Loop the script 
    bool m_looping;
    
    bool m_isPlaying;
    vpr::Thread* m_playThread;

};
}
}
}
#endif ///VES_CONDUCTOR_UTIL_DATALOGGERENGINE
