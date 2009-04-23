
// --- VE-Suite Includes --- //
#include "MySQLConnection.h"

// --- C/C++ Includes --- //


////////////////////////////////////////////////////////////////////////////////
MySQLConnection::MySQLConnection()
:
DBConnection(),
mysqlpp::Connection( false )
{
    m_dbType = MYSQL;
}
////////////////////////////////////////////////////////////////////////////////
MySQLConnection::~MySQLConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////

/*
//Retrieve a subset of the table and display it
mysqlpp::Query query = conn.query( "select Name from employees" );
if( mysqlpp::StoreQueryResult res = query.store() )
{
	std::cout << "We have:" << std::endl;
	mysqlpp::StoreQueryResult::const_iterator it;
	for( it = res.begin(); it != res.end(); ++it )
    {
		mysqlpp::Row row = *it;
		std::cout << '\t' << row[ 0 ] << std::endl;
	}
}
else
{
	std::cerr << "Failed to get item list: "
              << query.error() << std::endl;
}
*/
