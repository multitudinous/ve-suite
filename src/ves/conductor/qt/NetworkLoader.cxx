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

#include <ves/conductor/qt/NetworkLoader.h>
#include <ves/conductor/qt/XMLDataBufferEngine.h>
#include <ves/conductor/qt/UserPreferencesDataBuffer.h>

#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/network/VE_i.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/OneDDoubleArray.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>

#include <ves/util/commands/Minerva.h>


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
    // TODO: This code needs a thorough cleanup since it is mostly ripped from
    // other files and pasted in here. 
    
    // TODO: The Aspen-specific section needs to be uncommented and made 
    // functional.
    
    using ves::open::xml::Command;
    using ves::open::xml::CommandPtr;
    using ves::open::xml::DataValuePair;
    using ves::open::xml::DataValuePairPtr;
    using ves::open::xml::OneDDoubleArray;
    using ves::open::xml::OneDDoubleArrayPtr;
    using ves::open::xml::User;
    using ves::open::xml::UserPtr;
    
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
    {
        //Now load the xml data now that we are in the correct directory
        //canvas->PopulateNetworks( fileName );
        // RPT: Following line replaces key functionality of canvas->PopulateNetworks
        XMLDataBufferEngine::instance()->LoadVESData( fileName );
        
        //create hierarchy page
        //hierarchyTree->PopulateTree();

        ///This code will be moved in the future. It is Aspen specific code.
//         CommandPtr aspenBKPFile = UserPreferencesDataBuffer::instance()->
//             GetCommand( "Aspen_Plus_Preferences" );
//         
//         if( aspenBKPFile->GetCommandName() != "NULL" )
//         {
//             DataValuePairPtr bkpPtr =
//             aspenBKPFile->GetDataValuePair( "BKPFileName" );
//             std::string bkpFilename;
//             bkpPtr->GetData( bkpFilename );
//             OpenSimulation( wxString( bkpFilename.c_str(), wxConvUTF8 ) );
//         }
        
//         wxCommandEvent submitEvent;
//         SubmitToServer( submitEvent );
           // RPT: Following block duplicates key code from SubmitToServer
           const std::string nw_str = XMLDataBufferEngine::instance()->
                         SaveVESData( std::string( "returnString" ) );
           //serviceList->SetNetwork( nw_str );
           ves::xplorer::network::GraphicalPluginManager::instance()->
                                GetCORBAInterface()->SetNetworkString( nw_str.c_str() ); 
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
//         
//         if( recordScenes )
//         {
//             recordScenes->_buildPage();
//         }
    }
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

    //newCanvas = false;
}

} // namespace conductor
} // namespace ves