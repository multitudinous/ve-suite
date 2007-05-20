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
 * Date modified: $Date: 2007-02-27 22:54:03 -0600 (Tue, 27 Feb 2007) $
 * Version:       $Rev: 7013 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/StateInfo.h"

#include "VE_Open/XML/Model/Link.h"

#include <sstream>

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
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command& XMLDataBufferEngine::GetCommand( std::string commandKey )
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
                                      VE_XML::Command& command )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::Command >& 
XMLDataBufferEngine::GetCommandMap( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    return m_commandMap;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::SetCommandMap( std::map< std::string, 
                                         VE_XML::Command >& tempMap )
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
    
    // Just clear the design canvas
    //while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) { ; }
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
    networkWriter.ReadXMLData( xmlNetwork, dataToObtain );
    std::vector< VE_XML::XMLObject* >::iterator objectIter;
    std::vector< VE_XML::XMLObject* > objectVector = 
        networkWriter.GetLoadedXMLObjects();
    
    // we are expecting that a network will be found
    if ( !objectVector.empty() )
    {
        m_networkMap[ "Network" ] = 
            *dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
        objectVector.erase( objectVector.begin() );
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
    m_networkModelMap[ "Network" ] = networkModelVector;
    
    // now lets create a list of them
    size_t i = 0;
    std::ostringstream modelID;
    for( objectIter = objectVector.begin(); objectIter != objectVector.end(); )
    {
        ++i;
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
    
    if ( !objectVector.empty() )
    {
        VE_XML::User* userColor = 
            dynamic_cast< VE_XML::User* >( objectVector.at( 0 ) );
        //Set user preferences
        std::vector< VE_XML::Command* > tempStates = 
            userColor->GetUserStateInfo()->GetStateVector();
        std::map< std::string, VE_XML::Command > tempMap;
        for ( size_t i = 0; i < tempStates.size(); ++i )
        {
            VE_XML::Command* tempCommand = tempStates.at( i );
            tempMap[ tempCommand->GetCommandName() ] = (*tempCommand); 
        }
        //UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
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
        &m_userMap[ "User" ], "User" ) );
    
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    
    return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void XMLDataBufferEngine::NewVESData( bool promptClearXplorer )
{
    //Erase all the maps
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::VE_Model::Network& XMLDataBufferEngine::GetXMLNetworkDataObject( std::string dataNumber )
{
    return m_networkMap[ dataNumber ];
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string >& XMLDataBufferEngine::GetXMLModelDataObject( std::string dataNumber )
{
    return m_networkModelMap[ dataNumber ];
}


