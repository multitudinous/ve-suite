/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// This file has to be included first on Windows, or the compiler will complain about u_int in an ACE header file.
#include <ves/xplorer/network/VE_i.h>

#include <ves/conductor/qt/NetworkLoader.h>

#include <ves/conductor/qt/XMLDataBufferEngine.h>
#include <ves/conductor/qt/UserPreferencesDataBuffer.h>

#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/OneDDoubleArray.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/util/commands/Minerva.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

namespace ves
{
namespace conductor
{
        

NetworkLoader::NetworkLoader(  )
{
}

NetworkLoader::~NetworkLoader()
{
}

void NetworkLoader::LoadVesFile( const std::string& fileName )
{
    ///First lets get the current working directory and see where we need to
    ///change it to the new ves filename.
    boost::filesystem::path dir_path( fileName, boost::filesystem::native );
    try
    {
        if( !boost::filesystem::exists( dir_path ) )
        {
            return;
        }
    }
    catch( ... )
    {
        return;
    }
    std::string newWorkingDir = dir_path.parent_path().string();
    std::cout << "|\tThe new working directory is " 
        << newWorkingDir << std::endl;

#ifdef WIN32
    //http://msdn.microsoft.com/en-us/library/bf7fwze1(VS.80).aspx
    _chdir( newWorkingDir.c_str() );
#else
    chdir( newWorkingDir.c_str() );
#endif
    //A new working directory also means that 
    //the STORED scenes are no longer valid
    //ves::xplorer::EnvironmentHandler::instance()->GetTeacher()->Reset();

    // A change in the working dir also requires that we connect to the db file
    // in the new working dir and reset it.
    // UPDATE 2011-02-25: Moving to unified dir for db, so no path change for
    // db here. Still reset it since we're loading a new .ves.
//    std::string newDBPath = newWorkingDir;
//    newDBPath += "/ves.db";
//    ves::xplorer::data::DatabaseManager::instance()->SetDatabasePath( newDBPath );
    ves::xplorer::data::DatabaseManager::instance()->ResetAll();
    
    // TODO: This code needs a thorough cleanup since it is mostly ripped from
    // other files and pasted in here. 
    
    // TODO: The Aspen-specific section needs to be uncommented and made 
    // functional.

    using namespace ves::open::xml;
    
    {
        // Let xplorer know we are loading a new ves file so that it can do any
        // necessary cleanup, such as resetting the database
        CommandPtr loadVesFile( new Command() );
        loadVesFile->SetCommandName( "LOAD_VES_FILE" );
        // Dummy DVP to prevent crashes since xplorer assumes existence of 
        // valid DVP without testing.
        DataValuePairPtr nullDVP( new DataValuePair() );
        nullDVP->SetData( "LOAD_VES_FILE", "NULL" );
        loadVesFile->AddDataValuePair( nullDVP );
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( loadVesFile );

        //Send a new start position for all apps
        //do this before loading the ves data
        // so in case a file has a start position it will be used
        CommandPtr viewPointGUIData( new Command() );
        viewPointGUIData->SetCommandName( "Navigation_Data" );
        
        DataValuePairPtr quatStartPosition( new DataValuePair());
        OneDDoubleArrayPtr quatData( new OneDDoubleArray( 0 ) );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 0 );
        quatData->AddElementToArray( 1 );
        quatStartPosition->SetData( "QUAT_START_POSITION", quatData );
        viewPointGUIData->AddDataValuePair( quatStartPosition );
        
        DataValuePairPtr positionStartPosition( new DataValuePair() );
        OneDDoubleArrayPtr positionsData( new OneDDoubleArray( 0 ) );
        positionsData->AddElementToArray( 0 );
        positionsData->AddElementToArray( 0 );
        positionsData->AddElementToArray( 0 );
        positionStartPosition->SetData( "POSITION_START_POSITION", positionsData );
        viewPointGUIData->AddDataValuePair( positionStartPosition );
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( viewPointGUIData );
    }
    {
        //Reset the center point when a new app is loaded
        //do this first in case a file has a default center point it will be used
        CommandPtr centerPointUpdateData( new Command() );
        centerPointUpdateData->SetCommandName( "CENTER_POINT_UPDATE" );
        
        DataValuePairPtr resetDVP( new DataValuePair() );
        resetDVP->SetData( "CENTER_POINT_UPDATE_DVP", "Reset" );
        centerPointUpdateData->AddDataValuePair( resetDVP );
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( centerPointUpdateData );
    }

//+!
    //Reloading plugins
    //av_modules->ResetPluginTree();
    // RPT: The following block appears to be the key code from AvailableModules::ResetPluginTree
    DataValuePairPtr dvp( new DataValuePair( std::string( "STRING" ) ) );
    dvp->SetData( "Reload_Plugin_Objects", "Reload" );
    ves::open::xml::CommandPtr vec( new ves::open::xml::Command() );
    vec->SetCommandName( "Plugin_Control" );
    vec->AddDataValuePair( dvp );
    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( vec );
//-!

//+!
//     if( newCanvas )
//     {
//         //clear any current tree
//         hierarchyTree->Clear();
//         SetTitle( _( "VE-Suite: www.vesuite.org" ) );
//         canvas->CreateDefaultNetwork();
//     }
//     else
    //{
    //Now load the xml data now that we are in the correct directory
    //canvas->PopulateNetworks( fileName );
    // RPT: Following line replaces key functionality of canvas->PopulateNetworks
    XMLDataBufferEngine::instance()->LoadVESData( fileName );

    //create hierarchy page
    //hierarchyTree->PopulateTree();

    ///This code will be moved in the future. It is Aspen specific code.
    CommandPtr aspenBKPFile = UserPreferencesDataBuffer::instance()->
    GetCommand( "Aspen_Plus_Preferences" );

    if( aspenBKPFile->GetCommandName() != "NULL" )
    {
        DataValuePairPtr bkpPtr =
        aspenBKPFile->GetDataValuePair( "BKPFileName" );
        std::string bkpFilename;
        bkpPtr->GetData( bkpFilename );
        //OpenSimulation( wxString( bkpFilename.c_str(), wxConvUTF8 ) );

        //wxFileName bkpFileName;
        //bkpFileName.SetName( simName );

        CommandPtr returnState ( new Command() );
        returnState->SetCommandName( "openSimulation" );
        DataValuePairPtr data( new DataValuePair() );
        data->SetData( "AspenPlus", "openSimulation" );
        returnState->AddDataValuePair( data );

        data = DataValuePairPtr( new DataValuePair() );
        //data->SetData( "BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
        data->SetData( "BKPFileName",  bkpFilename.c_str() );
        returnState->AddDataValuePair( data );

        std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
        nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
        XMLReaderWriter commandWriter;
        std::string status = "returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );

        //Get results
        //std::string nw_str = serviceList->Query( status );
        std::string nw_str =
                ves::xplorer::network::GraphicalPluginManager::instance()->
                    GetCORBAInterface()->QueryCE( status );
        // We don't need to log anything
        //Log( nw_str.c_str() );
        // RPT: What does this variable do? Does not appear to do anything
        // based on code in apps/conductor/AppFrame.cxx
        //AspenSimOpen = true;
    }

//         wxCommandEvent submitEvent;
//         SubmitToServer( submitEvent );
// RPT: Following block duplicates key code from SubmitToServer
//{
    const std::string nw_str = XMLDataBufferEngine::instance()->
                    SaveVESData( std::string( "returnString" ) );
    //serviceList->SetNetwork( nw_str ); <-- old conductor
    //ves::xplorer::network::GraphicalPluginManager::instance()->
    //              GetCORBAInterface()->SetNetworkString( nw_str );
    // <-- CORBA version with Qt
    ves::xplorer::network::GraphicalPluginManager::instance()->
            SetCurrentNetwork( nw_str );
    std::vector< double > xplorerColor;
    xplorerColor.push_back( 0.0 );
    xplorerColor.push_back( 0.0 );
    xplorerColor.push_back( 0.0 );
    xplorerColor.push_back( 1.0 );
    DataValuePairPtr dataValuePair( new DataValuePair() );
    dataValuePair->SetData( std::string( "Load Data" ), xplorerColor );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "veNetwork Update" ) );
    veCommand->AddDataValuePair( dataValuePair );
    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( veCommand );
    // RPT: Make the first model in the network active
    ves::open::xml::model::SystemPtr system = XMLDataBufferEngine::instance()->GetXMLSystemDataObject( XMLDataBufferEngine::instance()->GetTopSystemId( ) );

    if( system->GetNumberOfModels() != 0 )
    {
        std::string modelID = system->GetModel( 0 )->GetID();

        ves::open::xml::DataValuePairPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "CHANGE_ACTIVE_MODEL", modelID );

        ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
        veCommand->SetCommandName( std::string( "CHANGE_ACTIVE_MODEL" ) );
        veCommand->AddDataValuePair( dataValuePair );

        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( veCommand );
    }
    //If this .ves has an associated .db, load it
    //We can't do this here because the previous commands to actually load
    //the model haven't been processed yet, and the resync operation kicked off
    //here assumes the model exists. To get around this, we set a flag in the UI
    //when we call into this loader. Then in our handler for ActiveModelChangedSignal
    //there, we look for this flag. If it's set, we do the equivalent of this
    //next block of code. It works, but it really puts the logic in the wrong
    //place.
//    const std::string& db( system->GetDBReference() );
//    if( !db.empty() )
//    {
//        std::cout << "NetworkLoader db: " << db << std::endl << std::flush;
//        ves::xplorer::data::DatabaseManager::instance()->LoadFrom( db );
//    }


           //}
//         
//         if( recordScenes )
//         {
//             recordScenes->_buildPage();
//         }
    //}
//-!
    
    //Now manage the data that is user specific to this ves file
    UserPtr userInfo = 
        XMLDataBufferEngine::instance()->GetXMLUserDataObject( "Network" );
    
    //If there was no color data in the ves file
    if( !userInfo->GetUserStateInfo() )
    {
        ///Color vector
        std::vector<double> backgroundColor;
        backgroundColor.clear();
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 1.0f );
        
        DataValuePairPtr dataValuePair( new DataValuePair() );
        dataValuePair->SetData( std::string( "Background Color" ), backgroundColor );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_BACKGROUND_COLOR" ) );
        veCommand->AddDataValuePair( dataValuePair );
        UserPreferencesDataBuffer::instance()->
            SetCommand( std::string( "CHANGE_BACKGROUND_COLOR" ), veCommand );
    }
   
    {
        // Create the command and data value pairs
        CommandPtr tempCommand = UserPreferencesDataBuffer::instance()->
            GetCommand( "CHANGE_BACKGROUND_COLOR" );
        
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( tempCommand );
        
        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "Navigation_Data" );
        
        if( tempCommand->GetCommandName().compare( "NULL" ) )
        {
            ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( tempCommand );
        }

        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "CHANGE_NEAR_FAR_RATIO" );

        if( tempCommand->GetCommandName().compare( "NULL" ) )
        {
            ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( tempCommand );
        }
        
        // Create the command and data value pairs
        tempCommand = 
            UserPreferencesDataBuffer::instance()->
            GetCommand( "Update LOD Scale" );
        if( tempCommand->GetCommandName().compare( "NULL" ) )
        {
            ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( tempCommand );
        }
    }
    
    //Send the new commands after the new data is loaded not before
    //Change view to CAD to make sure
    {
        DataValuePairPtr dataValuePair( new DataValuePair( std::string( "STRING" ) ) );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "CHANGE_XPLORER_VIEW" ) );
        dataValuePair->SetData( "CHANGE_XPLORER_VIEW", "CHANGE_XPLORER_VIEW_CAD" );
        veCommand->AddDataValuePair( dataValuePair );
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( veCommand );
    }    
    
    // Initialze Minerva.
    {
      CommandPtr earthCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_EARTH_COMMAND_NAME ) );
      if ( earthCommand )
      {
        ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( earthCommand );

        CommandPtr rasterGroupCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_RASTER_GROUP ) );
        CommandPtr elevationGroupCommand ( UserPreferencesDataBuffer::instance()->GetCommand ( ves::util::commands::ADD_ELEVATION_GROUP ) );

        if ( rasterGroupCommand && rasterGroupCommand->GetNumberOfDataValuePairs() > 0  )
        {
          ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( rasterGroupCommand );
        }

        if ( elevationGroupCommand && elevationGroupCommand->GetNumberOfDataValuePairs() > 0 )
        {
          ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( elevationGroupCommand );
        }

        //MinervaDialog* dialog ( this->GetMinervaDialog() );
        //dialog->InitalizeFromCommands ( elevationGroupCommand, rasterGroupCommand );
      }
    }
}

} // namespace conductor
} // namespace ves
