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
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/StateInfo.h"
#include "VE_Open/XML/StateInfoPtr.h"

#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/System.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Tag.h"
#include "VE_Open/XML/Model/TagPtr.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/UserPtr.h"
#include "VE_Open/XML/Command.h"

#include <sstream>
#include <algorithm>

using namespace VE_XML;
using namespace VE_Conductor;

vprSingletonImp( XMLDataBufferEngine );
////////////////////////////////////////////////////////////////////////////////
XMLDataBufferEngine::XMLDataBufferEngine( void )
{ 
   VE_XML::CommandWeakPtr nullCommand = new VE_XML::Command();
   nullCommand->SetCommandName( "NULL" );
   m_commandMap[ "NULL" ] = nullCommand;

    //Setup default system
    VE_XML::VE_Model::SystemStrongPtr tempSystem = 
       new VE_XML::VE_Model::System();
    m_systemMap[tempSystem->GetID()] = tempSystem;
    //get the main systems id
    topId = tempSystem->GetID();
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::CleanUp( void )
{
    m_commandMap.clear();
    m_networkMap.clear();
    m_modelMap.clear();
    m_networkModelMap.clear();
    m_userMap.clear();
    m_tagMap.clear();
    m_systemMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::CommandWeakPtr XMLDataBufferEngine::GetCommand( std::string commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, VE_XML::CommandStrongPtr >::iterator iter;
    iter = m_commandMap.find( commandKey );
    if ( iter == m_commandMap.end() )
    {
        return m_commandMap[ "NULL" ];
    }
    return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommand( std::string commandKey, 
                                      VE_XML::CommandWeakPtr command )
{
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::CommandWeakPtr > 
    XMLDataBufferEngine::GetCommandMap( void )
{
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, VE_XML::CommandWeakPtr > 
        tempMap( m_commandMap.begin(), m_commandMap.end() );
    return tempMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommandMap( std::map< std::string, 
                                         VE_XML::CommandWeakPtr > tempMap )
{
    m_commandMap.clear();
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap = std::map< std::string, VE_XML::CommandStrongPtr >( 
        tempMap.begin(), tempMap.end() );
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::LoadVESData( std::string xmlNetwork )
{
    if ( xmlNetwork.empty() )
    {
        return;
    }
    
    //Clean up all the maps so that everything is set for the new network data
    CleanUp();
    
    // Just clear the design canvas
    // Start the busy cursor
    // Load from the nt file loaded through wx
    // Get a list of all the command elements   
    //_fileProgress->Update( 10, _("start loading") );
    VE_XML::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();

    if ( xmlNetwork.size() < 512 )
    {
        networkWriter.ReadFromFile();
    }
    else
    {
        networkWriter.ReadFromString();
    }
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veNetwork" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veModel" ) );
    dataToObtain.push_back( std::make_pair( "XML", "User" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    networkWriter.ReadXMLData( xmlNetwork, dataToObtain );
    std::vector< VE_XML::XMLObject* >::iterator objectIter;
    std::vector< VE_XML::XMLObject* > objectVector = 
        networkWriter.GetLoadedXMLObjects();
    VE_XML::VE_Model::SystemStrongPtr tempSystem = 0;
    
    // we are expecting that a network will be found
    if ( !objectVector.empty() )
    {
        tempSystem = 
            dynamic_cast< VE_XML::VE_Model::System* >( objectVector.at( 0 ) );
        if( tempSystem )
        {
			m_systemMap[tempSystem->GetID()] = tempSystem;
			//get the main systems id
			topId = tempSystem->GetID();
            m_networkMap[ "Network" ] = tempSystem->GetNetwork();
        }
        else
        {
            tempSystem = new VE_XML::VE_Model::System();
			m_systemMap[tempSystem->GetID()] = tempSystem;
			topId = tempSystem->GetID();
            
            m_networkMap[ "Network" ] = 
                dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
            tempSystem->AddNetwork( m_networkMap[ "Network" ] );
            objectIter = objectVector.erase( objectVector.begin() );
            tempSystem = 0;
        }
    }
    else
    {
       std::cerr << "Improperly formated ves file." 
                    << "VES File Read Error" << std::endl;
    }
        

    std::vector< std::string > networkModelVector;
    std::vector< std::string >::iterator stringIter;
    long moduleID = 0;
    std::ostringstream fromID;
    std::ostringstream toID;
    VE_XML::VE_Model::NetworkWeakPtr tempNetwork = m_networkMap[ "Network" ];
    for( size_t i = 0; i < tempNetwork->GetNumberOfLinks(); ++i )
    {
        tempNetwork->GetLink( i )->GetFromModule()->GetData( moduleID );
        fromID << moduleID;
        tempNetwork->GetLink( i )->GetToModule()->GetData( moduleID );
        toID << moduleID;
        
        stringIter = std::find( networkModelVector.begin(), networkModelVector.end(), fromID.str() );
        if( stringIter == networkModelVector.end() )
        {
            networkModelVector.push_back( fromID.str() );
        }
        
        stringIter = std::find( networkModelVector.begin(), networkModelVector.end(), toID.str() );
        if( stringIter == networkModelVector.end() )
        {
            networkModelVector.push_back( toID.str() );
        }
        fromID.str("");
        toID.str("");
    }
        
    if( !tempSystem )
    {
        // now lets create a list of them
        std::ostringstream modelID;
        for( objectIter = objectVector.begin(); objectIter != objectVector.end(); )
        {
            VE_XML::VE_Model::ModelWeakPtr model = 
            dynamic_cast< VE_XML::VE_Model::Model* >( *objectIter );
            if( !model )
            {
                //if this object is not a model continue
                ++objectIter;
                continue;
            }
            objectIter = objectVector.erase( objectIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str() ] = model;
            modelID.str("");
        }        
    }
    else
    {
        // now lets create a list of them
        std::ostringstream modelID;
        std::vector< VE_XML::VE_Model::ModelWeakPtr >::iterator modelIter;
        std::vector< VE_XML::VE_Model::ModelWeakPtr > modelVector;
        modelVector = tempSystem->GetModels();
        
        for( modelIter = modelVector.begin(); modelIter != modelVector.end(); )
        {
            VE_XML::VE_Model::ModelWeakPtr model = *modelIter;

            modelIter = modelVector.erase( modelIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str() ] = model;
            stringIter = std::find( networkModelVector.begin(), 
                networkModelVector.end(), modelID.str() );
            if( stringIter == networkModelVector.end() )
            {
                networkModelVector.push_back( modelID.str() );
            }
            modelID.str("");
        }        
        objectIter = objectVector.erase( objectVector.begin() );
    }
    //For the case where there are no links between models
    //Just grab all the models in the ves file
    //this is somewhat of a hack but the schema does not support anything else
    if( tempNetwork->GetNumberOfLinks() == 0 )
    {
        std::ostringstream modelID;
        for( std::map< std::string, VE_XML::VE_Model::ModelStrongPtr >::iterator 
            modelIter = m_modelMap.begin(); modelIter != m_modelMap.end();
             ++modelIter )
        {
            modelID << modelIter->second->GetModelID();
            networkModelVector.push_back( modelID.str() );
            modelID.str("");
        }
    }
    m_networkModelMap[ "Network" ] = networkModelVector;
    
    if( !objectVector.empty() )
    {
        VE_XML::UserWeakPtr userColor = 
            dynamic_cast< VE_XML::User* >( objectVector.at( 0 ) );
        m_userMap[ "Network" ] = userColor;
        //Set user preferences
        std::vector< VE_XML::CommandWeakPtr > tempStates = 
            userColor->GetUserStateInfo()->GetStateVector();
        std::map< std::string, VE_XML::CommandWeakPtr > tempMap;
        for ( size_t i = 0; i < tempStates.size(); ++i )
        {
            //VE_XML::CommandWeakPtr tempCommand = tempStates.at( i );
            tempMap[ tempStates.at( i )->GetCommandName() ] = tempStates.at( i ); 
            //std::cout << " here " << tempCommand->GetCommandName() << std::endl;
        }
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
    }
    else
    {
        m_userMap[ "Network" ] = new VE_XML::User();
    }
    
    m_userMap[ "Network" ]->SetUserId( "User" );
    m_userMap[ "Network" ]->SetControlStatus( 
        VE_XML::User::VEControlStatus( "MASTER" ) );

	if(tempSystem)
	{
		//Parse out the remaining subsystems
		int modelCount = tempSystem->GetNumberOfModels();
		for ( size_t j = 0; j < modelCount; j++ )
		{
			if(tempSystem->GetModel(j)->GetSubSystem())
			{
				ParseSystem(tempSystem->GetModel(j)->GetSubSystem());
			}
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
std::string XMLDataBufferEngine::SaveVESData( std::string fileName )
{
    // Here we wshould loop over all of the following
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    //Write out the veUser info for the local user
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
            &(*m_systemMap[ topId]), "veSystem" ) );
    
    //Write out the veUser info for the local user
    VE_XML::StateInfoWeakPtr colorState = new VE_XML::StateInfo();
    ///Load the current preferences from the data buffer
    std::map< std::string, VE_XML::CommandWeakPtr > tempMap = 
        UserPreferencesDataBuffer::instance()->GetCommandMap();
    std::cout << tempMap.size() << std::endl;
    for( std::map< std::string, VE_XML::CommandWeakPtr >::iterator prefIter = 
         tempMap.begin(); prefIter != tempMap.end(); ++prefIter )
    {
        colorState->AddState( prefIter->second );
    }

    if( m_userMap.empty() )
    {
        m_userMap[ "Network" ] = new VE_XML::User();
        m_userMap[ "Network" ]->SetUserId( "User" );
        m_userMap[ "Network" ]->SetControlStatus( 
            VE_XML::User::VEControlStatus( "MASTER" ) );
    }
    m_userMap[ "Network" ]->SetStateInfo( colorState );

    
    //Write out the veUser info for the local user
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &(*m_userMap[ "Network" ]), "User" ) );
    
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    
    return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::NewVESData( bool promptClearXplorer )
{
    //Erase all the maps
    m_networkMap.clear();
    m_modelMap.clear();
    m_networkModelMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::VE_Model::NetworkWeakPtr XMLDataBufferEngine::GetXMLNetworkDataObject(
    std::string dataNumber )
{
    return m_networkMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::VE_Model::ModelWeakPtr > XMLDataBufferEngine::GetXMLModels()
{
    std::map< std::string, VE_XML::VE_Model::ModelWeakPtr > 
        tempMap( m_modelMap.begin(), m_modelMap.end() );
	return tempMap;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::VE_Model::ModelWeakPtr XMLDataBufferEngine::GetXMLModelDataObject( 
    std::string dataNumber )
{
    return m_modelMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::UserWeakPtr XMLDataBufferEngine::GetXMLUserDataObject( std::string dataNumber )
{
    return m_userMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > XMLDataBufferEngine::GetNetworkModelVector( 
    std::string dataNumber )
{
    //std::vector< std::string > temp = m_networkModelMap[ dataNumber ];
    //std::cout << " size " << temp.size() << std::endl;
    return m_networkModelMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
std::string XMLDataBufferEngine::GetTopSystemId( )
{
	return topId;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::VE_Model::SystemStrongPtr XMLDataBufferEngine::GetXMLSystemDataObject(
	std::string id )
{
	return m_systemMap[id];
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::VE_Model::SystemStrongPtr >XMLDataBufferEngine::
    GetXMLSystemDataMap( )
{
	return m_systemMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::ParseSystem( VE_XML::VE_Model::SystemWeakPtr system )
{
	//add the system to the map
	m_systemMap[system->GetID()] = system;

	//Parse out the subsystems
	int modelCount = system->GetNumberOfModels();
	for ( size_t j = 0; j < modelCount; j++ )
	{
		if(system->GetModel(j)->GetSubSystem())
		{
			ParseSystem(system->GetModel(j)->GetSubSystem());
		}
	}
}