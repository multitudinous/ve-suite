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
#include <ves/xplorer/data/DatabaseManager.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/xplorer/data/DatabaseDetailsPropertySet.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/data/CADSubNodePropertySet.h>
#include <ves/xplorer/data/CameraModePropertySet.h>
#include <ves/xplorer/data/CameraSettingsPropertySet.h>
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
//#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/DataException.h>

#include <boost/shared_ptr.hpp>
#include <boost/filesystem.hpp>
#include <boost/smart_ptr/shared_array.hpp>

#include <iostream>

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
    m_logger( Poco::Logger::get( "xplorer.data.DatabaseManager" ) ),
    m_logStream( new Poco::LogStream( m_logger ) )
{
    // Right now we use a null buffer and a null cache. These will be replaced
    // with actual caching and buffering strategies at some point in the future.
    m_cache =
            crunchstore::DataAbstractionLayerPtr( new crunchstore::NullCache );
    m_buffer =
            crunchstore::DataAbstractionLayerPtr( new crunchstore::NullBuffer );
    m_dataManager =
            crunchstore::DataManagerPtr( new crunchstore::DataManager );
    m_dataManager->SetCache( m_cache );
    m_dataManager->SetBuffer( m_buffer );
    m_workingStore = crunchstore::SQLiteStorePtr( new crunchstore::SQLiteStore );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_resyncFromDatabase ),
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
    if( m_dataManager && m_workingStore )
    {
        m_dataManager->DetachStore( m_workingStore );
    }
    //Remove working db file
    boost::filesystem::remove( m_path );
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::SetDatabasePath( const std::string& path )
{
    LOG_INFO( "SetDatabasePath " << path );

    if( !(m_dataManager && m_workingStore) )
    {
        return;
    }

    m_dataManager->DetachStore( m_workingStore );
    m_workingStore->SetStorePath( path );
    m_dataManager->AttachStore( m_workingStore,
                              crunchstore::Store::WORKINGSTORE_ROLE );
    m_path = path;
}
////////////////////////////////////////////////////////////////////////////////
//Poco::Data::SessionPool* DatabaseManager::GetPool()
//{
//    return mPool;
//}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > DatabaseManager::GetStringVector(
        const std::string& tableName,
        const std::string& columnName )
{
    std::vector< std::string > results;
    if( !TableExists( tableName ) )
    {
        //std::cout << "$$$ DatabaseManager::GetStringVector: Table " << tableName << " does not exist! (Looking for column " << columnName << ")" << std::endl << std::flush;
        return results;
    }

    std::vector< crunchstore::SearchCriterion > criteria;
    m_dataManager->Search( tableName, criteria, columnName, results );
//    std::cout << "DataManager::GetStringVector:\n\ttableName = " << tableName <<
//                 "\n\tcolumnName = " << columnName << "\n\tresults.size = " << results.size() << std::endl << std::flush;
    return results;

}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::TableExists( const std::string& tableName )
{
    if( !m_workingStore )
    {
        return false;
    }

    return m_workingStore->HasTypeName( tableName );
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::ResetAll()
{
    LOG_INFO( "ResetAll" );

    // Warning: This method is absolutely, positively specific to using an
    // sqlite store. We do this operation directly on the db rather than using
    // crunchstore's Remove() method on each entry, because that strategy would
    // be *very* slow. We could potentially use a combination of
    // crunchstore::Search() and crunchstore::Drop() to accomplish this, but
    // that would still be slower than doing it directly because crunchstore
    // does not support bulk transactions. Bulk transactions speed up this
    // process greatly.

    Poco::Data::Session session( m_workingStore->GetPool()->get() );
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
                if( ( tableName != "sqlite_sequence" ) && ( tableName != "XplorerDBDetails" ) )
                {
                    session << "DROP TABLE " << tableName, Poco::Data::now;
                }
            }
            session.commit();
        }
    }
    catch( Poco::Data::DataException& e )
    {
        LOG_ERROR( e.displayText() );
    }

    // Give everyone else a chance to alter their state to agree with a reset.
    m_resyncFromDatabase.signal();
}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::SaveAs( const std::string& path )
{
    LOG_INFO( "SaveAs " << path );
    // Warning: This assumes we are using sqlite for the working store. If we
    // switch to something else, this code will need to be re-thought.
    DatabaseDetailsPropertySet details;

    if( !TableExists( details.GetTypeName() ) )
    {
        details.SetPropertyValue( "DatabaseVersion", CURRENT_DB_VERSION );
        details.Save();
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
        LOG_ERROR( e.what() );
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
bool DatabaseManager::LoadFrom( const std::string& path )
{
    LOG_INFO( "LoadFrom " << path );
    // Warning: This assumes we are using sqlite for the working store. If we
    // switch to something else, this code will need to be re-thought.


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
    ids = GetStringVector( details.GetTypeName(), "uuid" );
    if( !ids.empty() )
    {
        details.SetUUID( ids.at( 0 ) );
        details.Load();
        if( details.PropertyExists( "DatabaseVersion" ) )
        {
            dbVersion = boost::any_cast<double>( details.GetPropertyValue( "DatabaseVersion" ) );
        }
    }
    if( dbVersion < CURRENT_DB_VERSION )
    {
        ConvertFromOld();
    }

    m_resyncFromDatabase.signal();

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::ConvertFromOld()
{
    LOG_INFO( "ConvertFromOld" );
    std::vector< propertystore::PropertySetPtr > propList;

    propList.push_back( propertystore::PropertySetPtr( new PreferencesPropertySet() ) );

    propList.push_back( propertystore::PropertySetPtr( new CADPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new CADSubNodePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new CameraModePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new CameraSettingsPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new ContourPlanePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new DatasetPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new IsosurfacePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new PolydataPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new StreamlinePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new VectorPlanePropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new VolumeVisPropertySet() ) );

    using namespace ves::xplorer::data::constraints;
    propList.push_back( propertystore::PropertySetPtr( new AngularSpringConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new LinearAndAngularSpringConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new LinearSpringConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new BallAndSocketConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new BoxConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new CardanConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new FixedConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new HingeConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new PlanarConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new RagdollConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new SliderConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new TwistSliderConstraintPropertySet() ) );
    propList.push_back( propertystore::PropertySetPtr( new WheelSuspensionConstraintPropertySet() ) );

    std::vector<std::string> ids;
    std::vector< propertystore::PropertySetPtr > properties;
    propertystore::PropertySetPtr propSet;

    for( size_t listIndex = 0; listIndex < propList.size(); ++listIndex )
    {
        propSet = propList.at( listIndex )->CreateNew();
        ids = DatabaseManager::instance()->GetStringVector( propSet->GetTypeName(), "uuid" );
        for( size_t index = 0; index < ids.size(); ++index )
        {
            propSet = propList.at( listIndex )->CreateNew();
            propSet->SetUUID( ids.at( index ) );
            propSet->Load();
            properties.push_back( propSet );
        }

        m_dataManager->Drop( propSet->GetTypeName(),
                            crunchstore::DataAbstractionLayer::WORKING_ROLE );
    }

    for( size_t index = 0; index < properties.size(); ++index )
    {
        properties.at( index )->Save();
    }
}
////////////////////////////////////////////////////////////////////////////////
crunchstore::SQLiteTransactionKey DatabaseManager::OpenBulkMode()
{
    return m_workingStore->BeginTransaction();
}
////////////////////////////////////////////////////////////////////////////////
void DatabaseManager::CloseBulkMode( crunchstore::SQLiteTransactionKey& key )
{
    std::cout << "CloseBulkMode" << std::endl << std::flush;
    m_workingStore->EndTransaction( key );
    std::cout << "\tCloseBulkMode done" << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
}// namespace data
}// namespace xplorer
}// namespace ves
