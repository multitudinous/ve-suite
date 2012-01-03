/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
// --- Poco includes --- //
#include <Poco/Data/SessionPool.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/DataException.h>

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

std::vector< std::string > DatabaseManager::GetStringVector( const std::string& tableName, const std::string& columnName, const std::string& searchCriteria, bool distinct )
{
    std::vector< std::string > returnValue;

    // If table doesn't exist, return an empty vector
    if( !TableExists( tableName ) )
    {
        return returnValue;
    }

    Poco::Data::Session session( mPool->get( ) );
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
        statement.execute( );
        Poco::Data::RecordSet recordset( statement );
        if( recordset.rowCount( ) != 0 )
        {
            for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
            {
                returnValue.push_back( recordset.value( 0, rowIndex ).convert<std::string > ( ) );
            }
        }
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }

    return returnValue;
}

bool DatabaseManager::TableExists( const std::string& tableName )
{
    bool exists = false;
    Poco::Data::Session session( mPool->get( ) );
    try
    {
        session << "SELECT 1 FROM sqlite_master WHERE name='" << tableName << "'",
                Poco::Data::into( exists ),
                Poco::Data::now;
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
        exists = false;
    }
    return exists;
}

void DatabaseManager::ResetAll( )
{
    Poco::Data::Session session( mPool->get( ) );
    Poco::Data::Statement statement( session );
    statement << "select name from sqlite_master where type = 'table'";

    try
    {
        statement.execute( );
        Poco::Data::RecordSet recordset( statement );

        // Walk through list of all tables and delete data in each
        if( recordset.rowCount( ) != 0 )
        {
            // Wrap operations into a single transaction for speed
            session.begin();
            for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
            {
                std::string tableName = recordset.value( 0, rowIndex ).convert< std::string > ( );
                if( tableName != "sqlite_sequence" )
                {
                    session << "DROP TABLE " << tableName, Poco::Data::now;
                }
            }
            session.commit();
        }
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
}

}// namespace data
}// namespace xplorer
}// namespace ves
