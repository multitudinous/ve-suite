
// --- VE-Suite Includes --- //
#include "MySQLConnection.h"

// --- MySQL++ Includes --- //
#include <query.h>

// --- C/C++ Includes --- //

////////////////////////////////////////////////////////////////////////////////
MySQLConnection::MySQLConnection(
    std::string& db,
    std::string& server,
    std::string& username,
    std::string& password,
    unsigned int port )
:
DBConnection( db ),
mysqlpp::Connection(
    db.c_str(), server.c_str(), username.c_str(), password.c_str(), port )
{
    m_dbType = MYSQL;

    if( connected() )
    {
        //Tell MySQL to use the desired database
        mysqlpp::Query theQuery = query();
        theQuery << "use " << m_name;
        theQuery.exec();

        //Get the list of tables for this database
        QueryTables();
    }
}
////////////////////////////////////////////////////////////////////////////////
MySQLConnection::~MySQLConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MySQLConnection::QueryTables()
{
    mysqlpp::Query theQuery = query( "show tables" );
    if( mysqlpp::StoreQueryResult tables = theQuery.store() )
    {
	    mysqlpp::StoreQueryResult::const_iterator itr;
	    for( itr = tables.begin(); itr != tables.end(); ++itr )
        {
		    mysqlpp::Row row = *itr;
		    m_tableNames.push_back( row[ 0 ].c_str() );
	    }
    }
    else
    {
	    std::cerr << "Failed to get table list: "
                  << theQuery.error() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
