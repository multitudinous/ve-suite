
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
        db.c_str(), server.c_str(), username.c_str(), password.c_str(), port ),
    m_query( query() )
{
    m_dbType = MYSQL;

    if( connected() )
    {
        //Tell MySQL to use the desired database
        m_query << "use " << m_name;
        m_query.exec();

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
    m_query << "show tables";
    if( mysqlpp::StoreQueryResult tables = m_query.store() )
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
                  << m_query.error() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
const StringArray2D* const MySQLConnection::GetTableDetails(
    std::string& tableName )
{
    std::map< std::string, StringArray2D >::const_iterator itr =
        m_tableDetails.find( tableName );
    if( itr != m_tableDetails.end() )
    {
        return &itr->second;
    }

    //Get the table details if we have not done so already
    m_query << "describe " << tableName;
    if( mysqlpp::StoreQueryResult res = m_query.store() )
    {
        StringArray2D tableDetails(
            res.num_rows(), StringArray1D( res.num_fields(), "" ) );
        for( size_t j = 0; j < res.num_fields(); ++j )
        {
            for( size_t i = 0; i < res.num_rows(); ++i )
            {
                tableDetails[ i ][ j ] = res[ i ][ j ].c_str();
            }
        }

        m_tableDetails[ tableName ] = tableDetails;
        itr = m_tableDetails.find( tableName );
        if( itr != m_tableDetails.end() )
        {
            return &itr->second;
        }
    }


    std::cerr << "Failed to get table details: "
              << m_query.error()
              << std::endl;

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
