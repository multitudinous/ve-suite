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

// --- VE-Suite Includes --- //
#include "MySQLConnection.h"

// --- POCO Includes --- //
#include <Poco/Data/Session.h>
#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/MySQL/Connector.h>
#include <Poco/Data/MySQL/MySQLException.h>

// --- C/C++ Includes --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
MySQLConnection::MySQLConnection(
    std::string& db,
    std::string& server,
    std::string& username,
    std::string& password,
    unsigned int port )
    :
    DBConnection( db )
{
    m_dbType = MYSQL;

    Poco::Data::MySQL::Connector::registerConnector();
    std::ostringstream connectionString;
    connectionString << "host=" << server << ";"
                     << "port=" << port << ";"
                     << "user=" << username << ";"
                     << "password=" << password << ";"
                     << "db=" << db << ";"
                     << "compress=true;"
                     << "auto-reconnect=true";
    try
    {
        m_session =
            new Poco::Data::Session(
                Poco::Data::SessionFactory::instance().create(
                    Poco::Data::MySQL::Connector::KEY, connectionString.str() ) );
    }
    catch( Poco::Data::MySQL::MySQLException& ex )
    {
        std::cout << "!!! WARNING: Connection failed. MySQL tests will fail !!!" << std::endl;
        std::cout << ex.displayText() << std::endl;
    }

    if( m_session && m_session->isConnected() )
    {
        std::cout << "*** Connected to " << '(' << connectionString.str() << ')' << std::endl;

        m_connected = true;

        m_statement = new Poco::Data::Statement( *m_session );

        //Get the list of tables for this database
        QueryTables();
    }
}
////////////////////////////////////////////////////////////////////////////////
MySQLConnection::~MySQLConnection()
{
    Poco::Data::MySQL::Connector::unregisterConnector();
}
////////////////////////////////////////////////////////////////////////////////
void MySQLConnection::QueryTables()
{
    std::string tableName;
    *m_statement << "show tables from " << m_name,
        Poco::Data::into( tableName ), Poco::Data::range( 0, 1 );
    while( !m_statement->done() )
    {
        m_statement->execute();
        m_tableNames.push_back( tableName.c_str() );
    }
}
////////////////////////////////////////////////////////////////////////////////
/*
const Poco::Data::RecordSet* const MySQLConnection::GetTableFieldNames(
    std::string& tableName )
{
    std::map< std::string, StringVector1D >::const_iterator itr =
        m_tableFieldNames.find( tableName );
    if( itr != m_tableFieldNames.end() )
    {
        return &itr->second;
    }

    //Get the table field names if we have not done so already
    m_query << "select * from " << tableName;
    if( mysqlpp::StoreQueryResult res = m_query.store() )
    {
        StringVector1D tableFieldNames( res.field_names()->size(), "" );
        for( size_t i = 0; i < res.field_names()->size(); ++i )
        {
            tableFieldNames[ i ] = res.field_name( i );
        }

        m_tableFieldNames[ tableName ] = tableFieldNames;
        itr = m_tableFieldNames.find( tableName );
        if( itr != m_tableFieldNames.end() )
        {
            return &itr->second;
        }
    }

    std::cerr << "Failed to get table field names: "
              << m_query.error()
              << std::endl;

    return NULL;
}
*/
////////////////////////////////////////////////////////////////////////////////
const StringVector2D* const MySQLConnection::GetTableDetails(
    std::string& tableName )
{
    std::map< std::string, StringVector2D >::const_iterator itr =
        m_tableDetails.find( tableName );
    if( itr != m_tableDetails.end() )
    {
        return &itr->second;
    }

    //Get the table details if we have not done so already
    Poco::Data::Statement theStatement( *m_session );
    theStatement << "describe " << tableName;
    theStatement.execute();
    Poco::Data::RecordSet rs( theStatement );
    
    StringVector2D tableDetails(
        rs.rowCount(), StringVector1D( rs.columnCount(), "" ) );
    for( size_t j = 0; j < rs.columnCount(); ++j )
    {
        for( size_t i = 0; i < rs.rowCount(); ++i )
        {
            tableDetails[ i ][ j ] = rs[ i ][ j ].convert< std::string >();
        }
    }

    m_tableDetails[ tableName ] = tableDetails;
    itr = m_tableDetails.find( tableName );
    if( itr != m_tableDetails.end() )
    {
        return &itr->second;
    }

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
/*
const Poco::Data::RecordSet* const MySQLConnection::GetTableData(
    std::string& tableName )
{
    std::map< std::string, StringVector2D >::const_iterator itr =
        m_tableData.find( tableName );
    if( itr != m_tableData.end() )
    {
        return &itr->second;
    }

    //Get the table details if we have not done so already
    m_query << "select * from " << tableName;
    if( mysqlpp::StoreQueryResult res = m_query.store() )
    {
        StringVector2D tableData(
            res.num_rows(), StringVector1D( res.num_fields(), "" ) );
        for( size_t j = 0; j < res.num_fields(); ++j )
        {
            for( size_t i = 0; i < res.num_rows(); ++i )
            {
                tableData[ i ][ j ] = res[ i ][ j ].c_str();
            }
        }

        m_tableData[ tableName ] = tableData;
        itr = m_tableData.find( tableName );
        if( itr != m_tableData.end() )
        {
            return &itr->second;
        }
    }

    std::cerr << "Failed to get table data: "
              << m_query.error()
              << std::endl;

    return NULL;
}
*/
////////////////////////////////////////////////////////////////////////////////
