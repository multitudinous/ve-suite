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

#include <ves/conductor/qt/NetworkLoader.h>

#include <ves/conductor/qt/XMLDataBufferEngine.h>
#include <ves/conductor/qt/UserPreferencesDataBuffer.h>

#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/eventmanager/EventFactory.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/OneDDoubleArray.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/util/commands/Minerva.h>

#include <boost/filesystem/operations.hpp>
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
////////////////////////////////////////////////////////////////////////////////
NetworkLoader::NetworkLoader()
{
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_dbPresent ),
        "NetworkLoader.dbPresent" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_navReset ),
        "ResetNavToGlobalOrigin" );
}
////////////////////////////////////////////////////////////////////////////////
NetworkLoader::~NetworkLoader()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkLoader::LoadVesFile( const std::string& fileName )
{
    ///First lets get the current working directory and see where we need to
    ///change it to the new ves filename.
    boost::filesystem::path dir_path( fileName );
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
    ///Since the file is good change the working directory
    std::string newWorkingDir = dir_path.parent_path().string();
    std::cout << "|\tThe new working directory is "
              << newWorkingDir << std::endl;

    m_filename = dir_path.filename().string();

#ifdef WIN32
    //http://msdn.microsoft.com/en-us/library/bf7fwze1(VS.80).aspx
    _chdir( newWorkingDir.c_str() );
#else
    int stopWarningMe = chdir( newWorkingDir.c_str() );
    if( stopWarningMe != 0 )
    {
        std::cout << "Could not change working directory." << std::endl;
    }
#endif
    using namespace ves::xplorer;
    reinterpret_cast< ves::util::StringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "WorkingDirectoryChanged" ) )
    ->signal( newWorkingDir );

    ///Now lets reset the view back to 0,0,0
    m_navReset.signal();

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
    //    ves::xplorer::data::DatabaseManager::instance()->ResetAll();

    // TODO: This code needs a thorough cleanup since it is mostly ripped from
    // other files and pasted in here.

    // TODO: The Aspen-specific section needs to be uncommented and made
    // functional.

    using namespace ves::open::xml;

    {
        // Let xplorer know we are loading a new ves file so that it can do any
        // necessary cleanup, such as resetting the database
        reinterpret_cast< ves::util::StringSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "VesFileLoading" ) )
        ->signal( m_filename );

        //Send a new start position for all apps
        //do this before loading the ves data
        // so in case a file has a start position it will be used
        CommandPtr viewPointGUIData( new Command() );
        viewPointGUIData->SetCommandName( "Navigation_Data" );

        DataValuePairPtr quatStartPosition( new DataValuePair() );
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

    // RPT: Following line replaces key functionality of canvas->PopulateNetworks
    XMLDataBufferEngine::instance()->LoadVESData( m_filename );
    ves::open::xml::model::SystemPtr system =
        XMLDataBufferEngine::instance()->
        GetXMLSystemDataObject( XMLDataBufferEngine::instance()->GetTopSystemId( ) );

    std::string const& db( system->GetDBReference() );
    if( !db.empty() )
    {
        m_dbPresent.signal( true );
    }
    else
    {
        m_dbPresent.signal( false );
    }

    ///This code will be moved in the future. It is Aspen specific code.
    /*CommandPtr aspenBKPFile = UserPreferencesDataBuffer::instance()->
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

        CommandPtr returnState( new Command() );
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
    }*/

    const std::string nw_str = XMLDataBufferEngine::instance()->
                               SaveVESData( std::string( "returnString" ) );

    ///Load the ves data on the xplorer side
    ves::xplorer::network::GraphicalPluginManager::instance()->
    SetCurrentNetwork( nw_str );

    // This signal replaces the above block. xplorerColor doesn't appear to be
    // processed anywhere, so was left out of the signal.
    reinterpret_cast< ves::util::VoidSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "UpdateNetwork" ) )
    ->signal( );

    // RPT: Make the first model in the network active
    // Connect to the ActiveModelChangedSignal. Just below we will send in a command
    // that will eventually trigger this signal. The slot called looks for a related
    // database file and tells xplorer to load it up. We can't simply call
    // DatabaseManager::LoadFrom here because the system and model may not actually
    // be loaded yet.
    //    CONNECTSIGNAL_1( "ModelHandler.ActiveModelChangedSignal",
    //                     void ( const std::string& ),
    //                     &NetworkLoader::OnActiveModelChanged,
    //                     m_connections, normal_Priority );

    if( system->GetNumberOfModels() != 0 )
    {
        std::string const modelID = system->GetModel( 0 )->GetID();

        reinterpret_cast< ves::util::StringSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "ChangeActiveModel" ) )
        ->signal( modelID );
    }

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

        reinterpret_cast< ves::util::BoolAndDoubleVectorSignal_type* >
        ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "PreferencesPropertySet.UsePreferredBackgroundColor" ) )
        ->signal( true, backgroundColor );
    }

    {
        if( UserPreferencesDataBuffer::instance()->GetCommand( "CHANGE_BACKGROUND_COLOR" )->GetCommandName().compare( "NULL" ) )
        {
            // Create the command and data value pairs
            CommandPtr tempCommand = UserPreferencesDataBuffer::instance()->
                                     GetCommand( "CHANGE_BACKGROUND_COLOR" );
            DataValuePairPtr activeModelDVP = tempCommand->GetDataValuePair( "Background Color" );
            std::vector< double > color;
            activeModelDVP->GetData( color );

            //ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( tempCommand );
            reinterpret_cast< ves::util::BoolAndDoubleVectorSignal_type* >
            ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "PreferencesPropertySet.UsePreferredBackgroundColor" ) )
            ->signal( true, color );
        }

        // Create the command and data value pairs
        CommandPtr tempCommand =
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
        CommandPtr earthCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_EARTH_COMMAND_NAME ) );
        if( earthCommand )
        {
            ves::xplorer::command::CommandManager::instance()->AddXMLCommand( earthCommand );

            CommandPtr rasterGroupCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_RASTER_GROUP ) );
            CommandPtr elevationGroupCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_ELEVATION_GROUP ) );

            if( rasterGroupCommand && rasterGroupCommand->GetNumberOfDataValuePairs() > 0 )
            {
                ves::xplorer::command::CommandManager::instance()->AddXMLCommand( rasterGroupCommand );
            }

            if( elevationGroupCommand && elevationGroupCommand->GetNumberOfDataValuePairs() > 0 )
            {
                ves::xplorer::command::CommandManager::instance()->AddXMLCommand( elevationGroupCommand );
            }

            //MinervaDialog* dialog ( this->GetMinervaDialog() );
            //dialog->InitalizeFromCommands ( elevationGroupCommand, rasterGroupCommand );
        }
    }

    OnActiveModelChanged( "null" );
}

// At the moment, this shouldn't need to be a separate function since we're
// no longer connecting to the ActiveModelChanged signal. Leaving it as-is
// for a bit in case we discover a bug in the current arrangement and need to
// go back to listening for ActiveModelChanged signal.
void NetworkLoader::OnActiveModelChanged( const std::string& )
{
    ves::xplorer::Model* model =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();
    ves::open::xml::model::SystemPtr system =
        model->GetModelData()->GetParentSystem();
    std::string const& db( system->GetDBReference() );
    if( !db.empty() )
    {
        ves::xplorer::data::DatabaseManager::instance()->LoadFrom( db );
    }

    // Get id of every available CADPropertySet in the db so we can load it
    // to turn on physics, animations, and other goodies that are stored in
    // the db but are not in the .ves xml file.
    ves::xplorer::data::CADPropertySet temp;
    std::vector<std::string> ids =
        ves::xplorer::data::DatabaseManager::instance()->
        GetStringVector( temp.GetTypeName(), "uuid" );

    // Iterate through each available set and load it from db
    std::vector<std::string>::const_iterator idIter = ids.begin();
    while( idIter != ids.end() )
    {
        ves::xplorer::data::CADPropertySet cadSet;
        ves::xplorer::data::CADPropertySet tcadSet;

        // Turn on all live properties. This will ensure that physics meshes
        // and all the other good stuff is turned on too. Note that dynamics
        // data would be loaded and run even without this call since it is
        // a permanently live property -- i.e. not one made live with
        // MakeLive.

        // To eliminate excess creation of PhysicsRigidBodies, we
        // first we load the CADPropertySet into tcadset so we can
        // read whether physics is enabled. Then we set physics to true on
        // cadSet, turn on live properties, and then set physics false on
        // cadSet. This ensures that physics is completely turned off before we
        // start touching physics properties in the propertyset. After the set
        // has been loaded and the live physics properties have been dealt
        // with, we turn physics back on. The slot for turning physics on will
        // in turn set all the other physics properties on the rigid body.
        tcadSet.SetUUID( *idIter );
        tcadSet.Load( );
        bool physics = boost::any_cast<bool>( tcadSet.GetPropertyValue( "Physics_Enable" ) );
        cadSet.SetUUID( *idIter );
        cadSet.SetPropertyValue( "Physics_Enable", true );
        cadSet.EnableLiveProperties( true );
        cadSet.SetPropertyValue( "Physics_Enable", false );
        cadSet.Load( );
        if( physics )
        {
            cadSet.SetPropertyValue( "Physics_Enable", true );
        }
        ++idIter;
    }

    // Notify of completion of this file load
    reinterpret_cast< ves::util::StringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "VesFileLoaded" ) )
    ->signal( m_filename );

    //Autodestruct
    delete this;
}

} // namespace conductor
} // namespace ves
