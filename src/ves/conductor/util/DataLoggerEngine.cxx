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
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/util/DataLoggerEngine.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <vpr/Util/Timer.h>
#include <vpr/Thread/Thread.h>
#include <vpr/System.h>
#include <vpr/vprParam.h>

#include <boost/bind.hpp>

#include <sstream>
#include <fstream>

using namespace ves::open::xml;
using namespace ves::conductor::util;

vprSingletonImp( DataLoggerEngine );

////////////////////////////////////////////////////////////////////////////////
DataLoggerEngine::DataLoggerEngine( void )
    :
    m_commandTimer( new vpr::Timer() ),
    m_dataLogging( false ),
    m_looping( false ),
    m_playThread( 0 ),
    m_isPlaying( false )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DataLoggerEngine::~DataLoggerEngine()
{    
    delete m_commandTimer;
    m_commandTimer = 0;
    m_looping = false;
    
    if( m_playThread )
    {
        try
        {
            m_playThread->kill();
        }
        catch ( ... )
        {
            ;//do nothing
        }
        delete m_playThread;   
    } 
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::CleanUp()
{
    if( m_dataLoggerCommandVectorQueue.size() > 0 )
    {
        WriteFile();
    }
}
////////////////////////////////////////////////////////////////////////////////
bool DataLoggerEngine::SendCommandStringToXplorer( const ves::open::xml::CommandWeakPtr& veCommand )
{
    if( m_dataLogging )
    {
        //Now send the data to xplorer
        ves::open::xml::XMLReaderWriter netowrkWriter;
        netowrkWriter.UseStandaloneDOMDocumentManager();
        
        // New need to destroy document and send it
        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( veCommand.lock(), "vecommand" ) );
        std::string xmlDocument( "returnString" );
        netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

        m_commandTimer->stopTiming();

        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Time" ), m_commandTimer->getLastTiming() );
        DataValuePairPtr actionDataValuePair( new DataValuePair() );
        actionDataValuePair->SetData( std::string( "Xplorer Action" ), veCommand.lock() );
        CommandPtr actionCommand( new Command() );
        actionCommand->SetCommandName( std::string( "Action Pair" ) );
        actionCommand->AddDataValuePair( dataValuePair );
        actionCommand->AddDataValuePair( actionDataValuePair );
        
        m_dataLoggerCommandVectorQueue.push_back( actionCommand );

        m_commandTimer->reset();
        m_commandTimer->startTiming();
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool DataLoggerEngine::SendNetworkStringToCE( const std::string& command )
{
    if( m_dataLogging )
    {
        m_commandTimer->stopTiming();
        
        XMLReaderWriter networkWriter;
        networkWriter.UseStandaloneDOMDocumentManager();
        networkWriter.ReadFromString();
        
        // do this for models
        networkWriter.ReadXMLData( command, "System", "veSystem" );
        std::vector< XMLObjectPtr > objectVector =
            networkWriter.GetLoadedXMLObjects();
        
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Time" ), m_commandTimer->getLastTiming() );
        DataValuePairPtr actionDataValuePair( new DataValuePair() );
        actionDataValuePair->SetData( std::string( "SetNetwork" ), boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) ) );
        CommandPtr actionCommand( new Command() );
        actionCommand->SetCommandName( std::string( "Action Pair" ) );
        actionCommand->AddDataValuePair( dataValuePair );
        actionCommand->AddDataValuePair( actionDataValuePair );
        
        m_dataLoggerCommandVectorQueue.push_back( actionCommand );
        
        m_commandTimer->reset();
        m_commandTimer->startTiming();
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::LoadVEMFile( const std::string& file )
{    
    m_dataLogging = false;

    m_loadedCommands.clear();
    
    //Now send the data to xplorer
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromFile();
    networkWriter.ReadXMLData( file, "Command", "vecommand" );
    m_loadedCommands = networkWriter.GetLoadedXMLObjects();
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::PlayVEMFile()
{
    if( m_playThread )
    {
        try
        {
            m_playThread->kill();
        }
        catch ( ... )
        {
            ;//do nothing
        }
        delete m_playThread;   
    }
    
    m_playThread = 
        new vpr::Thread( boost::bind( &DataLoggerEngine::PlayThread, this ) );
    
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::ToggleOn( bool turnOn )
{
    m_dataLogging = turnOn;
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::LoopingOn( bool looping )
{
    m_looping = looping;
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::SetMovieFilename( const std::string& filename )
{
    m_movieFilename = filename;
    m_dataLogging = true;
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::WriteFile()
{
    // New need to destroy document and send it
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    for( size_t i = 0; i < m_dataLoggerCommandVectorQueue.size(); ++i )
    {
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( m_dataLoggerCommandVectorQueue.at( i ), "vecommand" ) );
    }
    m_dataLoggerCommandVectorQueue.clear();
    
    //std::string xmlDocument( "returnString" );
    //Now send the data to xplorer
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, m_movieFilename.c_str(), "Command" );
    
    {
        //std::cout << "Writing VE movie file " 
        //    << m_movieFilename << "." << std::endl;
        //std::ofstream commandScriptfile( m_movieFilename.c_str() );
        //commandScriptfile << xmlDocument << std::endl;
        //commandScriptfile.close();
    }
}
////////////////////////////////////////////////////////////////////////////////
void DataLoggerEngine::PlayThread()
{
    m_isPlaying = true;
    do
    {
        for( size_t i = 0; i < m_loadedCommands.size(); ++i )
        {
            ves::open::xml::CommandPtr tempCmd = boost::dynamic_pointer_cast<ves::open::xml::Command >( m_loadedCommands.at( i ) );
            DataValuePairPtr timeDvp = tempCmd->GetDataValuePair( 0 );
            DataValuePairPtr actionDvp = tempCmd->GetDataValuePair( 1 );
            //sleep
            double sleepTime;
            timeDvp->GetData( sleepTime );
            vpr::System::sleep( vpr::Uint32( sleepTime ) );
            //now send command the appropriate place
            if( actionDvp->GetDataName() == "Xplorer Action" )
            {
                ves::conductor::util::CORBAServiceList::instance()->
                SendCommandStringToXplorer( 
                                           boost::dynamic_pointer_cast<ves::open::xml::Command >( 
                                                                                                 actionDvp->GetDataXMLObject() ) );
            }
            else if( actionDvp->GetDataName() == "SetNetwork" )
            {
                ves::open::xml::model::SystemPtr tempSys = boost::dynamic_pointer_cast<ves::open::xml::model::System >( 
                                                                                                                       actionDvp->GetDataXMLObject() );
                std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
                nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( tempSys, "veSystem" ) );
                
                std::string xmlDocument( "returnString" );
                //Now send the data to xplorer
                ves::open::xml::XMLReaderWriter netowrkWriter;
                netowrkWriter.UseStandaloneDOMDocumentManager();
                netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Network" );
                
                ves::conductor::util::CORBAServiceList::instance()->
                SetNetwork( xmlDocument );
            }
        }
    }
    while( m_looping );    
    m_isPlaying = false;
}
////////////////////////////////////////////////////////////////////////////////
bool DataLoggerEngine::IsPlaying()
{
    return m_isPlaying;
}
