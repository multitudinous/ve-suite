
// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- POCO Includes --- //
#include <Poco/Data/Session.h>

// --- C/C++ Includes --- //


////////////////////////////////////////////////////////////////////////////////
DBConnection::DBConnection( std::string& name )
    :
    m_connected( false ),
    m_name( name )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DBConnection::~DBConnection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
const bool DBConnection::Connected() const
{
    return m_connected;
}
////////////////////////////////////////////////////////////////////////////////
const unsigned int DBConnection::GetDBType() const
{
    return m_dbType;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& DBConnection::GetName() const
{
    return m_name;
}
////////////////////////////////////////////////////////////////////////////////
const StringVector1D& DBConnection::GetTableNames() const
{
    return m_tableNames;
}
////////////////////////////////////////////////////////////////////////////////
