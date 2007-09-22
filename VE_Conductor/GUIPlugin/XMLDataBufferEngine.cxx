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

#include <sstream>
#include <algorithm>

using namespace VE_XML;
using namespace VE_Conductor;

vprSingletonImp( XMLDataBufferEngine );
////////////////////////////////////////////////////////////////////////////////
XMLDataBufferEngine::XMLDataBufferEngine( void )
{ 
   VE_XML::Command nullCommand;
   nullCommand.SetCommandName( "NULL" );
   m_commandMap[ "NULL" ] = nullCommand;
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
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command XMLDataBufferEngine::GetCommand( std::string commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, VE_XML::Command >::iterator iter;
    iter = m_commandMap.find( commandKey );
    if ( iter == m_commandMap.end() )
    {
        return m_commandMap[ "NULL" ];
    }
    return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommand( std::string commandKey, 
                                      VE_XML::Command command )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::Command > 
XMLDataBufferEngine::GetCommandMap( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    return m_commandMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommandMap( std::map< std::string, 
                                         VE_XML::Command > tempMap )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap = tempMap;
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
    VE_XML::VE_Model::System* tempSystem = 0;
    
    // we are expecting that a network will be found
    if ( !objectVector.empty() )
    {
        tempSystem = 
            dynamic_cast< VE_XML::VE_Model::System* >( objectVector.at( 0 ) );
        if( tempSystem )
        {
            m_networkMap[ "Network" ] = *(tempSystem->GetNetwork());
        }
        else
        {
            m_networkMap[ "Network" ] = 
                *dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
            objectIter = objectVector.erase( objectVector.begin() );
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
    for( size_t i = 0; i < m_networkMap[ "Network" ].GetNumberOfLinks(); ++i )
    {
        m_networkMap[ "Network" ].GetLink( i )->GetFromModule()->GetData( moduleID );
        fromID << moduleID;
        m_networkMap[ "Network" ].GetLink( i )->GetToModule()->GetData( moduleID );
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
            VE_XML::VE_Model::Model* model = 
            dynamic_cast< VE_XML::VE_Model::Model* >( *objectIter );
            if( !model )
            {
                //if this object is not a model continue
                ++objectIter;
                continue;
            }
            objectIter = objectVector.erase( objectIter );
            modelID << model->GetModelID();
            m_modelMap[ modelID.str() ] = *model;
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
            m_modelMap[ modelID.str() ] = *model;
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
    if( m_networkMap[ "Network" ].GetNumberOfLinks() == 0 )
    {
        std::ostringstream modelID;
        std::map< std::string, VE_XML::VE_Model::Model >::iterator modelIter;
        for( modelIter = m_modelMap.begin(); modelIter != m_modelMap.end();
             ++modelIter )
        {
            modelID << modelIter->second.GetModelID();
            networkModelVector.push_back( modelID.str() );
            modelID.str("");
        }
    }
    m_networkModelMap[ "Network" ] = networkModelVector;
    
    if( !objectVector.empty() )
    {
        VE_XML::UserPtr userColor = 
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
    
}
////////////////////////////////////////////////////////////////////////////////
std::string XMLDataBufferEngine::SaveVESData( std::string fileName )
{
    // Here we wshould loop over all of the following
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    //  Newtork
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &m_networkMap[ "Network" ], "veNetwork" ) );
    //  Models
    std::map< std::string, VE_XML::VE_Model::Model >::iterator iter;
    for ( iter=m_modelMap.begin(); iter!=m_modelMap.end(); ++iter )
    {
        nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
            &iter->second, "veModel" ) );
    }
    //  tags
    /*for ( size_t i = 0; i < veTagVector.size(); ++i )
    {
        delete veTagVector.at( i );
    }
    veTagVector.clear();
    
    for ( size_t i = 0; i < tags.size(); ++i )
    {
        std::pair< unsigned int, unsigned int > pointCoords;
        
        veTagVector.push_back( new VE_Model::Tag( doc ) );
        
        veTagVector.back()->SetTagText( tags.back().text.c_str() );
        
        pointCoords.first = tags.back().cons[0].x;
        pointCoords.second = tags.back().cons[0].y;
        veTagVector.back()->GetTagPoint( 0 )->SetPoint( pointCoords );
        
        pointCoords.first = tags.back().cons[1].x;
        pointCoords.second = tags.back().cons[1].y;
        veTagVector.back()->GetTagPoint( 1 )->SetPoint( pointCoords );
        
        pointCoords.first = tags.back().box.x;
        pointCoords.second = tags.back().box.y;
        veTagVector.back()->GetTagPoint( 2 )->SetPoint( pointCoords );
    }
    
    for ( size_t i = 0; i < tags.size(); ++i )
    {
        doc->getDocumentElement()->appendChild
        ( 
          veTagVector.at( i )->GetXMLData( "veTag" )
          );
    }*/
   
    //Write out the veUser info for the local user
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &(*m_userMap[ "User" ]), "User" ) );
    
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
VE_XML::VE_Model::Network XMLDataBufferEngine::GetXMLNetworkDataObject(
    std::string dataNumber )
{
    return m_networkMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::VE_Model::Model > XMLDataBufferEngine::GetXMLModels()
{
	return m_modelMap;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::VE_Model::Model XMLDataBufferEngine::GetXMLModelDataObject( 
    std::string dataNumber )
{
    return m_modelMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::User XMLDataBufferEngine::GetXMLUserDataObject( std::string dataNumber )
{
    return (*m_userMap[ dataNumber ]);
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > XMLDataBufferEngine::GetNetworkModelVector( 
    std::string dataNumber )
{
    //std::vector< std::string > temp = m_networkModelMap[ dataNumber ];
    //std::cout << " size " << temp.size() << std::endl;
    return m_networkModelMap[ dataNumber ];
}
