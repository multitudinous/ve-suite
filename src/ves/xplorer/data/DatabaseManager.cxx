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
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/data/DatabaseDetailsPropertySet.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/data/CADSubNodePropertySet.h>
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/IsosurfacePropertySet.h>
#include <ves/xplorer/data/PolydataPropertySet.h>
#include <ves/xplorer/data/PreferencesPropertySet.h>
#include <ves/xplorer/data/StreamlinePropertySet.h>
#include <ves/xplorer/data/VectorPlanePropertySet.h>
#include <ves/xplorer/data/VolumeVisPropertySet.h>

#include <ves/xplorer/data/constraints/AngularSpringConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/LinearAndAngularSpringConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/LinearSpringConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/BallAndSocketConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/BoxConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/CardanConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/FixedConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/HingeConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/PlanarConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/RagdollConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/SliderConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/TwistSliderConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/WheelSuspensionConstraintPropertySet.h>

// --- Poco includes --- //
#include <Poco/Data/SessionPool.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/DataException.h>

#include <boost/shared_ptr.hpp>
#define BOOST_FILESYSTEM_VERSION 2
#include <boost/filesystem.hpp>

#include <iostream>
#include <boost/smart_ptr/shared_array.hpp>

namespace ves
{
namespace xplorer
{
namespace data
{

vprSingletonImp( DatabaseManager );
//vprSingletonImpLifetime( DatabaseManager, 0 );
////////////////////////////////////////////////////////////////////////////////
DatabaseManager::DatabaseManager()
    :
    mPool( 0 )
{
    eventmanager::EventManager::instance()->RegisterSignal(
            new eventmanager::SignalWrapper< boost::signals2::signal< void() > >( &m_resyncFromDatabase ),
       "DatabaseManager.ResyncFromDatabase" );
}
////////////////////////////////////////////////////////////////////////////////
DatabaseManager::~DatabaseManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::Shutdown()
{
    if( mPool )
    {
        //std::cout << "Number of idle Poco::Sessions " << mPool->idle() 
        //    << " Number of dead Poco::Sessions " << mPool->dead() << std::endl;
        //This must be deleted from the thread that it was created from
        delete mPool;
        mPool = 0;
    }
    try
    {
        Poco::Data::SQLite::Connector::unregisterConnector();
    }
    catch( ... )
    {
        ;
    }
    //Remove working db file
    boost::filesystem::remove( m_path );
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::SetDatabasePath( const std::string& path )
{
    if( mPool )
    {
        delete mPool;
        mPool = 0;
        Poco::Data::SQLite::Connector::unregisterConnector();
    }

    Poco::Data::SQLite::Connector::registerConnector();
    mPool = new Poco::Data::SessionPool( "SQLite", path, 1, 32, 10 );
    m_path = path;
}
////////////////////////////////////////////////////////////////////////////////
Poco::Data::SessionPool* DatabaseManager::GetPool()
{
    return mPool;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > DatabaseManager::GetStringVector( const std::string& tableName, const std::string& columnName, const std::string& searchCriteria, bool distinct )
{
    std::vector< std::string > returnValue;

    // If table doesn't exist, return an empty vector
    if( !TableExists( tableName ) )
    {
        return returnValue;
    }

    Poco::Data::Session session( mPool->get() );
    Poco::Data::Statement statement( session );

    // Build the following query: "SELECT [DISTINCT] columnName FROM tableName [WHERE searchCriteria]"
    statement << "SELECT ";
    if( distinct )
    {
        statement << "DISTINCT ";
    }
    statement << columnName << " FROM " << tableName;
    if( !searchCriteria.empty() )
    {
        statement << " WHERE " << searchCriteria;
    }

    try
    {
        statement.execute();
        Poco::Data::RecordSet recordset( statement );
        if( recordset.rowCount() != 0 )
        {
            for( size_t rowIndex = 0; rowIndex < recordset.rowCount(); rowIndex++ )
            {
                returnValue.push_back( recordset.value( 0, rowIndex ).convert<std::string > () );
            }
        }
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << "DatabaseManager::GetStringVector: " << e.displayText() << std::endl;
    }

    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::TableExists( const std::string& tableName )
{
    bool exists = false;
    Poco::Data::Session session( mPool->get() );
    try
    {
        session << "SELECT 1 FROM sqlite_master WHERE name='" << tableName << "'",
                Poco::Data::into( exists ),
                Poco::Data::now;
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText() << std::endl;
        exists = false;
    }
    return exists;
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::ResetAll()
{
    Poco::Data::Session session( mPool->get() );
    Poco::Data::Statement statement( session );
    statement << "select name from sqlite_master where type = 'table'";

    try
    {
        statement.execute();
        Poco::Data::RecordSet recordset( statement );

        // Walk through list of all tables and delete data in each
        if( recordset.rowCount() != 0 )
        {
            // Wrap operations into a single transaction for speed
            session.begin();
            for( size_t rowIndex = 0; rowIndex < recordset.rowCount(); rowIndex++ )
            {
                std::string tableName = recordset.value( 0, rowIndex ).convert< std::string > ();
                if( (tableName != "sqlite_sequence") && (tableName != "XplorerDBDetails") )
                {
                    session << "DROP TABLE " << tableName, Poco::Data::now;
                }
            }
            session.commit();
        }
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText() << std::endl;
    }

    // Give everyone else a chance to alter their state to agree with a reset.
    m_resyncFromDatabase();
}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::SaveAs( const std::string& path )
{
    DatabaseDetailsPropertySet details;

    if( !TableExists( details.GetTableName() ) )
    {
        details.SetPropertyValue( "DatabaseVersion", CURRENT_DB_VERSION );
        details.WriteToDatabase();
    }

    try
    {
        boost::filesystem::path from( m_path );
        boost::filesystem::path to( path );
        boost::filesystem::copy_file( from, to, boost::filesystem::copy_option::overwrite_if_exists );

        return true;
    }
    catch( std::exception& e )
    {
        std::cerr << e.what() << std::endl << std::flush;
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::LoadFrom( const std::string& path )
{
    // Order of events:
    // 1. Shutdown db, so we release everything.
    // 2. Copy the db file given in path over top of working db.
    // 3. Reconnect to working db.
    // 4. Emit ResyncFromDatabase signal so listeners know their data may have
    //    changed.

    Shutdown();
    try
    {
        boost::filesystem::path from( path );
        boost::filesystem::path to( m_path );
        boost::filesystem::copy_file( from, to, boost::filesystem::copy_option::overwrite_if_exists );

        //return true;
    }
    catch( std::exception& e )
    {
        std::cerr << e.what() << std::endl << std::flush;
        return false;
    }
    SetDatabasePath( m_path );

    // Check DB version. If it's older than current, load in all the core propertyset
    // types, wipe the respective tables, and write them back out again. This will
    // ensure that any changes to the core propertysets are reflected in the database.
    DatabaseDetailsPropertySet details;
    std::vector<std::string> ids;
    double dbVersion = 0.0;
    ids = DatabaseManager::instance()->GetStringVector( details.GetTableName(), "uuid" );
    if( !ids.empty() )
    {
        details.SetUUID( ids.at( 0 ) );
        details.LoadFromDatabase();
        if( details.PropertyExists( "DatabaseVersion" ) )
        {
            dbVersion = boost::any_cast<double>( details.GetPropertyValue("DatabaseVersion") );
        }
    }
    if( dbVersion < CURRENT_DB_VERSION )
    {
        ConvertFromOld();
    }

    m_resyncFromDatabase();

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::ConvertFromOld()
{
    std::vector<PropertySetPtr> propList;

    propList.push_back( PropertySetPtr( new PreferencesPropertySet() ) );

    propList.push_back( PropertySetPtr( new CADPropertySet() ) );
    propList.push_back( PropertySetPtr( new CADSubNodePropertySet() ) );
    propList.push_back( PropertySetPtr( new ContourPlanePropertySet() ) );
    propList.push_back( PropertySetPtr( new DatasetPropertySet() ) );
    propList.push_back( PropertySetPtr( new IsosurfacePropertySet() ) );
    propList.push_back( PropertySetPtr( new PolydataPropertySet() ) );
    propList.push_back( PropertySetPtr( new StreamlinePropertySet() ) );
    propList.push_back( PropertySetPtr( new VectorPlanePropertySet() ) );
    propList.push_back( PropertySetPtr( new VolumeVisPropertySet() ) );

    using namespace ves::xplorer::data::constraints;
    propList.push_back( PropertySetPtr( new AngularSpringConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new LinearAndAngularSpringConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new LinearSpringConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new BallAndSocketConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new BoxConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new CardanConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new FixedConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new HingeConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new PlanarConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new RagdollConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new SliderConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new TwistSliderConstraintPropertySet() ) );
    propList.push_back( PropertySetPtr( new WheelSuspensionConstraintPropertySet() ) );

    std::vector<std::string> ids;
    std::vector<PropertySetPtr> properties;
    PropertySetPtr propSet;

    for( size_t listIndex = 0; listIndex < propList.size(); ++listIndex )
    {
        propSet = propList.at( listIndex )->CreateNew();
        ids = DatabaseManager::instance()->GetStringVector( propSet->GetTableName(), "uuid" );
        for( size_t index = 0; index < ids.size(); ++index )
        {
            propSet = propList.at( listIndex )->CreateNew();
            propSet->SetUUID( ids.at( index ) );
            propSet->LoadFromDatabase();
            properties.push_back( propSet );
        }
        Poco::Data::Session session( mPool->get() );
        if(TableExists( propSet->GetTableName() ))
        {
            session << "DROP TABLE " << propSet->GetTableName(), Poco::Data::now;
        }
    }

    for( size_t index = 0; index < properties.size(); ++index )
    {
        properties.at( index )->WriteToDatabase();
    }
}
////////////////////////////////////////////////////////////////////////////////
}// namespace data
}// namespace xplorer
}// namespace ves
