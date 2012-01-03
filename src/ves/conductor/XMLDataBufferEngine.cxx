/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/open/xml/StateInfoPtr.h>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/UserPtr.h>
#include <ves/open/xml/Command.h>

#include <sstream>
#include <algorithm>

using namespace ves::open::xml;
using namespace ves::open::xml::model;
using namespace ves::conductor;

vprSingletonImp( XMLDataBufferEngine );
////////////////////////////////////////////////////////////////////////////////
XMLDataBufferEngine::XMLDataBufferEngine( void )
{
    //Setup default system
    ves::open::xml::model::SystemPtr tempSystem(
        new ves::open::xml::model::System());
    m_systemMap[tempSystem->GetID()] = tempSystem;
    //get the main systems id
    topId = tempSystem->GetID();
    
    m_userMap[ "Network" ] = 
        ves::open::xml::UserPtr( new ves::open::xml::User() );
    m_userMap[ "Network" ]->SetUserId( "User" );
    m_userMap[ "Network" ]->SetControlStatus( 
        ves::open::xml::User::VEControlStatus( "MASTER" ) );    
}
////////////////////////////////////////////////////////////////////////////////
XMLDataBufferEngine::~XMLDataBufferEngine()
{
    CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::CleanUp( void )
{
    m_systemMap.clear();
    m_networkMap.clear();
    m_modelMap.clear();
    m_networkModelMap.clear();
    m_tagMap.clear();
    m_userMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
/*ves::open::xml::CommandWeakPtr XMLDataBufferEngine::GetCommand( std::string commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, ves::open::xml::CommandPtr >::iterator iter;
    iter = m_commandMap.find( commandKey );
    if( iter == m_commandMap.end() )
    {
        return m_commandMap[ "NULL" ];
    }
    return iter->second;
}*/
////////////////////////////////////////////////////////////////////////////////
/*void XMLDataBufferEngine::SetCommand( std::string commandKey,
                                      ves::open::xml::CommandWeakPtr command )
{
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}*/
////////////////////////////////////////////////////////////////////////////////
/*std::map< std::string, ves::open::xml::CommandWeakPtr >
XMLDataBufferEngine::GetCommandMap( void )
{
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, ves::open::xml::CommandWeakPtr >
    tempMap( m_commandMap.begin(), m_commandMap.end() );
    return tempMap;
}*/
////////////////////////////////////////////////////////////////////////////////
/*void XMLDataBufferEngine::SetCommandMap( std::map < std::string,
                                         ves::open::xml::CommandWeakPtr > tempMap )
{
    m_commandMap.clear();
    //vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap = std::map< std::string, ves::open::xml::CommandPtr >(
                       tempMap.begin(), tempMap.end() );
}*/
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::LoadVESData( std::string xmlNetwork )
{
    if( xmlNetwork.empty() )
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
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();

    if( xmlNetwork.size() < 512 )
    {
        networkWriter.ReadFromFile();
    }
    else
    {
        networkWriter.ReadFromString();
    }
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veNetwork" ) );
    dataToObtain.push_back( std::make_pair( "Model", "veModel" ) );
    dataToObtain.push_back( std::make_pair( "XML", "User" ) );
    networkWriter.ReadXMLData( xmlNetwork, dataToObtain );
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();
    ves::open::xml::model::SystemPtr tempSystem;

    // we are expecting that a network will be found
    if( !objectVector.empty() )
    {
        //Conversion<T, U>::exists2Way
        //SameType<T, U>::value
        //Conversion<T, U>::exists
        //const XMLObjectPtr tempPtr = objectVector.at( 0 );
        
        //bool tempBool = Loki::IsSameType<Loki::TypeTraits::PointerTraits( tempPtr ), Loki::TypeTraits<SystemPtr>::PointeeType>::value;
        
        /*if( ::Loki::TypeTraits<SystemPtr>::ReferredType == ::Loki::TypeTraits<tempPtr>::ReferredType )
        {
            
        }*/
        //std::vector< ves::open::xml::XMLObjectPtr >::const_iterator constObjIter;
        //constObjIter = objectVector.begin();
        //tempSystem = SystemPtr( *constObjIter );
        //tempSystem = objectVector.at( 0 );
        //System* tempSysOld = dynamic_cast< System* >( &(*( *constObjIter )));
        //typeid
        //If the file is a new xml file with a system element
        if( objectVector.at( 0 )->GetObjectType() == "System" )
        //if( tempSystem )
        {
            tempSystem = boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) );
            m_systemMap[tempSystem->GetID()] = tempSystem;
            //get the main systems id
            topId = tempSystem->GetID();
            m_networkMap[ "Network" ] = tempSystem->GetNetwork();
            objectIter = objectVector.begin();
            objectIter = objectVector.erase( objectIter );
        }
        //Else if the file jsut has a netowrk and a series of models
        else
        {
            tempSystem = ves::open::xml::model::SystemPtr( new ves::open::xml::model::System() );
            m_systemMap[tempSystem->GetID()] = tempSystem;
            topId = tempSystem->GetID();

            m_networkMap[ "Network" ] = ves::open::xml::model::NetworkPtr( boost::dynamic_pointer_cast<Network>( objectVector.at( 0 ) ) );
            tempSystem->AddNetwork( m_networkMap[ "Network" ] );
            objectIter = objectVector.begin();
            objectIter = objectVector.erase( objectIter );
            tempSystem = ves::open::xml::model::SystemPtr();
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
    ves::open::xml::model::NetworkPtr tempNetwork = m_networkMap[ "Network" ];
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
        fromID.str( "" );
        toID.str( "" );
    }

    if( !tempSystem )
    {
        // now lets create a list of them
        std::ostringstream modelID;
        for( objectIter = objectVector.begin(); objectIter != objectVector.end(); )
        {
            if( (*objectIter)->GetObjectType() != "Model" )
            {
                //if this object is not a model continue
                ++objectIter;
                continue;
            }
            ves::open::xml::model::ModelPtr model = boost::dynamic_pointer_cast<Model>( *objectIter );
            objectIter = objectVector.erase( objectIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str()] = model;
            m_systemMap[ topId ]->AddModel( model );
            modelID.str( "" );
        }
    }
    else
    {
        // now lets create a list of them
        std::ostringstream modelID;
        std::vector< ves::open::xml::model::ModelPtr >::iterator modelIter;
        std::vector< ves::open::xml::model::ModelPtr > modelVector;
        modelVector = tempSystem->GetModels();

        for( modelIter = modelVector.begin(); modelIter != modelVector.end(); )
        {
            ves::open::xml::model::ModelPtr model = *modelIter;

            modelIter = modelVector.erase( modelIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str()] = model;
            stringIter = std::find( networkModelVector.begin(),
                                    networkModelVector.end(), modelID.str() );
            if( stringIter == networkModelVector.end() )
            {
                networkModelVector.push_back( modelID.str() );
            }
            modelID.str( "" );
        }
    }
   //For the case where there are no links between models
    //Just grab all the models in the ves file
    //this is somewhat of a hack but the schema does not support anything else
    if( tempNetwork->GetNumberOfLinks() == 0 )
    {
        std::ostringstream modelID;
        for( std::map< std::string, ves::open::xml::model::ModelPtr >::iterator
                modelIter = m_modelMap.begin(); modelIter != m_modelMap.end();
                ++modelIter )
        {
            modelID << modelIter->second->GetModelID();
            networkModelVector.push_back( modelID.str() );
            modelID.str( "" );
        }
    }
    m_networkModelMap[ "Network" ] = networkModelVector;

    if( !objectVector.empty() )
    {
        m_userMap[ "Network" ] = boost::dynamic_pointer_cast<ves::open::xml::User>( objectVector.at( 0 ) );
        //Set user preferences
        std::vector< ves::open::xml::CommandPtr > tempStates =
            m_userMap[ "Network" ]->GetUserStateInfo()->GetStateVector();
        std::map< std::string, ves::open::xml::CommandPtr > tempMap;
        for( size_t i = 0; i < tempStates.size(); ++i )
        {
            tempMap[ tempStates.at( i )->GetCommandName()] = tempStates.at( i );
        }
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
    }
    else
    {
        m_userMap[ "Network" ] = ves::open::xml::UserPtr( new ves::open::xml::User() );
    }

    m_userMap[ "Network" ]->SetUserId( "User" );
    m_userMap[ "Network" ]->SetControlStatus(
        ves::open::xml::User::VEControlStatus( "MASTER" ) );

    if( tempSystem )
    {
        //Parse out the remaining subsystems
        int modelCount = tempSystem->GetNumberOfModels();
        for( size_t j = 0; j < modelCount; j++ )
        {
            if( tempSystem->GetModel( j )->GetSubSystem() )
            {
                ParseSystem( tempSystem->GetModel( j )->GetSubSystem() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string XMLDataBufferEngine::SaveVESData( std::string fileName )
{
    // Here we wshould loop over all of the following
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    //Write out the veUser info for the local user
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >(
                         m_systemMap[ topId], "veSystem" ) );

    //Write out the veUser info for the local user
    ves::open::xml::StateInfoPtr colorState( new ves::open::xml::StateInfo() );
    ///Load the current preferences from the data buffer
    std::map< std::string, ves::open::xml::CommandPtr > tempMap =
        UserPreferencesDataBuffer::instance()->GetCommandMap();
    for( std::map< std::string, ves::open::xml::CommandPtr >::iterator prefIter =
                tempMap.begin(); prefIter != tempMap.end(); ++prefIter )
    {
        colorState->AddState( prefIter->second );
    }

    if( m_userMap.empty() )
    {
        m_userMap[ "Network" ] = ves::open::xml::UserPtr( new ves::open::xml::User() );
        m_userMap[ "Network" ]->SetUserId( "User" );
        m_userMap[ "Network" ]->SetControlStatus(
            ves::open::xml::User::VEControlStatus( "MASTER" ) );
    }
    m_userMap[ "Network" ]->SetStateInfo( colorState );


    //Write out the veUser info for the local user
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >(
                         m_userMap[ "Network" ], "User" ) );

    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );

    return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::NewVESData( bool promptClearXplorer )
{
    //Erase all the maps
    CleanUp();

    //Setup default system
    ves::open::xml::model::SystemPtr tempSystem(
        new ves::open::xml::model::System() );
    m_systemMap[ tempSystem->GetID()] = tempSystem;
    //get the main systems id
    topId = tempSystem->GetID();
    
    m_userMap[ "Network" ] = 
        ves::open::xml::UserPtr( new ves::open::xml::User() );
    m_userMap[ "Network" ]->SetUserId( "User" );
    m_userMap[ "Network" ]->SetControlStatus( 
        ves::open::xml::User::VEControlStatus( "MASTER" ) );
}
////////////////////////////////////////////////////////////////////////////////
/*ves::open::xml::model::NetworkPtr XMLDataBufferEngine::GetXMLNetworkDataObject(
    std::string dataNumber )
{
    return m_networkMap[ dataNumber ];
}*/
////////////////////////////////////////////////////////////////////////////////
/*ves::open::xml::model::ModelPtr XMLDataBufferEngine::GetXMLModelDataObject(
    std::string dataNumber )
{
    return m_modelMap[ dataNumber ];
}*/
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::UserPtr XMLDataBufferEngine::GetXMLUserDataObject( std::string dataNumber )
{
    return m_userMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
/*std::vector< std::string > XMLDataBufferEngine::GetNetworkModelVector(
    std::string dataNumber )
{
    return m_networkModelMap[ dataNumber ];
}*/
////////////////////////////////////////////////////////////////////////////////
std::string XMLDataBufferEngine::GetTopSystemId( )
{
    return topId;
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::model::SystemPtr XMLDataBufferEngine::GetXMLSystemDataObject(
    std::string id )
{
    return m_systemMap[id];
}
////////////////////////////////////////////////////////////////////////////////
const std::map< std::string, ves::open::xml::model::SystemPtr > 
    XMLDataBufferEngine::GetXMLSystemDataMap()
{
    return m_systemMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::ParseSystem( ves::open::xml::model::SystemPtr system )
{
    //add the system to the map
    AddSubSystem( system );

    //Parse out the subsystems
    int modelCount = system->GetNumberOfModels();
    for( size_t j = 0; j < modelCount; j++ )
    {
        if( system->GetModel( j )->GetSubSystem() )
        {
            ParseSystem( system->GetModel( j )->GetSubSystem() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool XMLDataBufferEngine::AddSubSystem( ves::open::xml::model::SystemPtr system )
{
    std::map< std::string, ves::open::xml::model::SystemPtr >::const_iterator 
        iter = m_systemMap.find( system->GetID() );
    if( iter != m_systemMap.end() )
    {
        std::cerr << "XMLDataBufferEngine::AddSubSystem : "
            << "System already on system map." << std::endl;
        return false;
    }
    
    m_systemMap[ system->GetID() ] = system;
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool XMLDataBufferEngine::RemoveModelFromSystem( ves::open::xml::model::ModelPtr model )
{
    std::map< std::string, ves::open::xml::model::SystemPtr >::const_iterator iter;
    for( iter = m_systemMap.begin(); iter != m_systemMap.end(); ++iter )
    {
        if( iter->second->RemoveModel( model ) )
        {
            ves::open::xml::model::SystemPtr tempSys = model->GetSubSystem();
            if( tempSys )
            {
                RemovemSystem( tempSys );
            }
            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool XMLDataBufferEngine::RemovemSystem( ves::open::xml::model::SystemPtr system )
{
    std::map< std::string, ves::open::xml::model::SystemPtr >::iterator iter;
    for( iter = m_systemMap.begin(); iter != m_systemMap.end(); ++iter )
    {
        if( iter->second == system )
        {
            m_systemMap.erase( iter );
            std::vector< ModelPtr > tempModels = system->GetModels();
            for( size_t i = 0; i < tempModels.size(); ++i )
            {
                ves::open::xml::model::SystemPtr tempSys = 
                    tempModels.at( i )->GetSubSystem();
                if( tempSys )
                {
                    RemovemSystem( tempSys ); 
                }
            }
            return true;
        }
    }
    return false;
}
