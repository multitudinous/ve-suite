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

#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <switchwire/SingleShotSignal.h>
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
    xplorer::eventmanager::EventFactory* factory = xplorer::eventmanager::EventFactory::instance();

    factory->GetSignalByType< ves::util::StringSignal_type >( "WorkingDirectoryChanged" )
        ->signal( newWorkingDir );

    ///Now lets reset the view back to 0,0,0
    m_navReset.signal();

    using namespace ves::open::xml;

    {
        // Let xplorer know we are loading a new ves file so that it can do any
        // necessary cleanup, such as resetting the database
        factory->GetSignalByType< ves::util::StringSignal_type >
                ( "VesFileLoading" )->signal( m_filename );

        //Send a new start position for all apps
        //do this before loading the ves data
        // so in case a file has a start position it will be used
        std::vector< double > quat;
        quat.push_back( 0.0 );
        quat.push_back( 0.0 );
        quat.push_back( 0.0 );
        quat.push_back( 1.0 );
        std::vector< double > position;
        position.push_back( 0.0 );
        position.push_back( 0.0 );
        position.push_back( 0.0 );

        // TEMP: I know this block called into DeviceHandler (DeviceSlots) not
        // EnvironmentHanlder (SetResetStartPositionEventHandler) because
        // the command package did not include the command name "SET_START_POSITION"
        factory->GetSignalBySignature
                < void( std::vector< double >&, std::vector< double >& ) >
                ( "SetNavigationData" )->signal( quat, position );
    }
    {
        //Reset the center point when a new app is loaded
        //do this first in case a file has a default center point it will be used
        factory->GetSignalByType< ves::util::StringSignal_type >
                ( "CenterPointUpdate" )->signal( "Reset" );
    }

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

    const std::string nw_str = XMLDataBufferEngine::instance()->
                               SaveVESData( std::string( "returnString" ) );


    ///Load the ves data on the xplorer side
    ves::xplorer::network::GraphicalPluginManager::instance()->
        SetCurrentNetwork( nw_str );

    factory->GetSignalByType< ves::util::VoidSignal_type >
            ( "UpdateNetwork" )->signal( );

    // Make the first model in the network active
    if( system->GetNumberOfModels() != 0 )
    {
        std::string const modelID = system->GetModel( 0 )->GetID();

        factory->GetSignalByType< ves::util::StringSignal_type >
                ( "ChangeActiveModel" )->signal( modelID );
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

        factory->GetSignalByType< ves::util::BoolAndDoubleVectorSignal_type >
                ( "PreferencesPropertySet.UsePreferredBackgroundColor" )->
                    signal( true, backgroundColor );
    }

    {
        if( UserPreferencesDataBuffer::instance()->GetCommand( "CHANGE_BACKGROUND_COLOR" )->GetCommandName().compare( "NULL" ) )
        {
            CommandPtr tempCommand = UserPreferencesDataBuffer::instance()->
                                     GetCommand( "CHANGE_BACKGROUND_COLOR" );
            DataValuePairPtr activeModelDVP = tempCommand->GetDataValuePair( "Background Color" );
            std::vector< double > color;
            color.push_back( 0.0 );
            color.push_back( 0.0 );
            color.push_back( 0.0 );
            activeModelDVP->GetData( color );

            factory->GetSignalByType< ves::util::BoolAndDoubleVectorSignal_type >
                    ( "PreferencesPropertySet.UsePreferredBackgroundColor" )
                        ->signal( true, color );
        }

        CommandPtr tempCommand =
            UserPreferencesDataBuffer::instance()->
            GetCommand( "Navigation_Data" );

        if( tempCommand->GetCommandName().compare( "SET_START_POSITION" ) )
        {
            // Does not contain SET_START_POSITION
            // Pull position and quat data and fire SetNavigationData signal
            std::vector< double > quat;
            quat.push_back( 0.0 );
            quat.push_back( 0.0 );
            quat.push_back( 0.0 );
            quat.push_back( 1.0 );
            std::vector< double > position;
            position.push_back( 0.0 );
            position.push_back( 0.0 );
            position.push_back( 0.0 );
            DataValuePairPtr quatPosition = tempCommand->GetDataValuePair( "QUAT_START_POSITION" );
            if( quatPosition )
            {
                OneDDoubleArrayPtr data = boost::dynamic_pointer_cast<OneDDoubleArray>( quatPosition->GetDataXMLObject() );
                quat = data->GetArray();
                quatPosition = tempCommand->GetDataValuePair( "POSITION_START_POSITION" );
                data = boost::dynamic_pointer_cast<OneDDoubleArray>( quatPosition->GetDataXMLObject() );
                position = data->GetArray();
            }

            factory->GetSignalBySignature
                    < void( std::vector< double >&, std::vector< double >& ) >
                    ( "SetNavigationData" )->signal( quat, position );
        }
        else // Contains SET_START_POSITION
        {
            // Fire SetRestartPosition signal
            factory->GetSignalBySignature
                    < void( ) >
                    ( "SetResetStartPosition" )->signal( );
        }

        tempCommand =
            UserPreferencesDataBuffer::instance()->
            GetCommand( "CHANGE_NEAR_FAR_RATIO" );

        DataValuePairPtr nfr = tempCommand->GetDataValuePair( "Near Far Ratio" );
        double nfrValue = 0.005;
        if( nfr )
        {
            nfr->GetData( nfrValue );
        }
        switchwire::SingleShotSignal< void, bool const&, double const& >
                ( "NetworkLoader.NearFarRatio", true, nfrValue );



        tempCommand = UserPreferencesDataBuffer::instance()->
                GetCommand( "Update LOD Scale" );

        ves::open::xml::DataValuePairPtr scaleValue =
            tempCommand->GetDataValuePair( "Geometry LOD Scale" );
        // Getting the value as a long (rather than double) comes directly
        // from GeometryLODScaleEventHandler.
        long alpha = 1;
        if( scaleValue )
        {
            scaleValue->GetData( alpha );
        }
        switchwire::SingleShotSignal< void, const double >
           ( "NetworkLoader.GeometryLODScale", static_cast< double >( alpha ) );
    }

    //Send the new commands after the new data is loaded not before
    //Change view to CAD to make sure
    {
        switchwire::SingleShotSignal< void, const std::string& >
                ( "ChangeXplorerView", "CAD" );
    }

    // Initialze Minerva.
    /*{
        CommandPtr earthCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_EARTH_COMMAND_NAME ) );
        if( earthCommand )
        {
            CommandPtr rasterGroupCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_RASTER_GROUP ) );
            CommandPtr elevationGroupCommand( UserPreferencesDataBuffer::instance()->GetCommand( ves::util::commands::ADD_ELEVATION_GROUP ) );

            if( rasterGroupCommand && rasterGroupCommand->GetNumberOfDataValuePairs() > 0 )
            {
            }

            if( elevationGroupCommand && elevationGroupCommand->GetNumberOfDataValuePairs() > 0 )
            {
            }

            //MinervaDialog* dialog ( this->GetMinervaDialog() );
            //dialog->InitalizeFromCommands ( elevationGroupCommand, rasterGroupCommand );
        }
    }*/

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
    factory->GetSignalByType< ves::util::StringSignal_type >( "VesFileLoaded" )
        ->signal( m_filename );

    //Autodestruct
    delete this;
}

} // namespace conductor
} // namespace ves
