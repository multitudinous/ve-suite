/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef XML_DATA_BUFFER_ENGINE_H
#define XML_DATA_BUFFER_ENGINE_H
/*!\file XMLDataBufferEngine.h
XMLDataBufferEngine API
*/
/*!\class VE_Conductor::XMLDataBufferEngine
* 
*/
#include "VE_Open/XML/Command.h"

//do this to remove compile warnings from linux platforms
#undef _REENTRANT
#include <vpr/Util/Singleton.h>
#include <vpr/Sync/Mutex.h>

#include <map>
#include <string>

#include <VE_Open/XML/Model/Model.h>
#include <VE_Open/XML/Model/Network.h>
#include <VE_Open/XML/Model/Tag.h>
#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/UserPtr.h>

#include "VE_Installer/include/VEConfig.h"
namespace VE_Conductor
{
class VE_GUIPLUGINS_EXPORTS XMLDataBufferEngine
{
private:
   /// Required so that vpr::Singleton can instantiate this class.
   /// friend class vpr::Singleton< UserPreferenceDataBuffer >;
   XMLDataBufferEngine( void );
   ~XMLDataBufferEngine(){ ; } //Never gets called, don't implement until vrj2.2
   vprSingletonHeader( XMLDataBufferEngine );
public:
    ///Desctructor call until vrj 2.2 is released
    void CleanUp( void );
    ///Get Command with key
    ///The key MUST be the command name
    ///\param commandKey command desired by the user
    VE_XML::Command GetCommand( std::string commandKey );
    ///set Command with key
    ///\param commandKey key of the command desired
    ///\param command command to be stored
    void SetCommand( std::string commandKey, VE_XML::Command command );
    ///Get all the commands
    std::map< std::string, VE_XML::Command > GetCommandMap( void );
    ///Set all the commands
    ///\param tempMap the the map of commands when initialized by the user
    void SetCommandMap( std::map< std::string, VE_XML::Command > tempMap );
    ///Set data from CORBA receiver thread
    void SetXplorerData(){ ; }
    ///Load data 
    void LoadVESData( std::string vesNetwork );
    ///Save data 
    std::string SaveVESData( std::string fileName );
    ///New 
    void NewVESData( bool promptClearXplorer );
    ///Get data
    VE_XML::VE_Model::Network GetXMLNetworkDataObject( std::string dataNumber );
	///Get the network
	std::map< std::string, VE_XML::VE_Model::Model > GetXMLModels();
    ///Get data
    VE_XML::VE_Model::Model GetXMLModelDataObject( std::string dataNumber );
    ///Get data
    std::vector< std::string > GetNetworkModelVector( std::string dataNumber );
    ///Get data
    VE_XML::User GetXMLUserDataObject( std::string dataNumber );
        
private:
    ///Map to store the command name and command for easy lookup by the user
    std::map< std::string, VE_XML::Command > m_commandMap;
    ///mutex to lock the command map so that it is accessed appropriately
    vpr::Mutex m_commandMapLock;
    ///Map
    std::map< std::string, VE_XML::VE_Model::Network > m_networkMap;
    //Map of model ids for the given network
    std::map< std::string, std::vector< std::string > > m_networkModelMap;
    ///Map
    std::map< std::string, VE_XML::VE_Model::Model > m_modelMap;
    ///Map
    std::map< std::string, VE_XML::VE_Model::Tag > m_tagMap;
    ///Map
    std::map< std::string, VE_XML::UserPtr > m_userMap;
};
}
#endif //XML_DATA_BUFFER_ENGINE_H
