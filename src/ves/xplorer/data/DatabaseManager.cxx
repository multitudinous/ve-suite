
// --- Poco includes --- //
#include <Poco/Data/SessionPool.h>
#include <Poco/Data/SQLite/Connector.h>

#include <boost/shared_ptr.hpp>

#include <iostream>
#include <boost/smart_ptr/shared_array.hpp>

#include "DatabaseManager.h"

namespace ves
{
namespace xplorer
{
namespace data
{

vprSingletonImp( DatabaseManager );

DatabaseManager::DatabaseManager( ) :
mPool( 0 )
{

}

DatabaseManager::~DatabaseManager( )
{
    if( mPool )
    {
        delete mPool;
    }
    Poco::Data::SQLite::Connector::unregisterConnector( );
}

void DatabaseManager::SetDatabasePath( const std::string& path )
{
    if( mPool )
    {
        delete mPool;
        Poco::Data::SQLite::Connector::unregisterConnector( );
    }

    Poco::Data::SQLite::Connector::registerConnector( );
    mPool = new Poco::Data::SessionPool( "SQLite", path );
}

Poco::Data::SessionPool* DatabaseManager::GetPool( )
{
    return mPool;
}

}// namespace data
}// namespace xplorer
}// namespace ves
