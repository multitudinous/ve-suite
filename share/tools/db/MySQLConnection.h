
#ifndef MY_SQL_CONNECTION_H
#define MY_SQL_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- POCO Includes --- //
#include <Poco/SharedPtr.h>

namespace Poco
{
namespace Data
{
class Statement;
}
}

// --- C/C++ Includes --- //

/*!\file MySQLConnection.h
 *
 */

/*!\class MySQLConnection
 * 
 */
class MySQLConnection : public DBConnection
{
public:
    ///Constructor
    MySQLConnection(
        std::string& db,
        std::string& server,
        std::string& username,
        std::string& password,
        unsigned int port );

    ///Destructor
    virtual ~MySQLConnection();

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableFieldNames(
        std::string& tableName );
    */

    ///
    virtual const StringVector2D* const GetTableDetails(
        std::string& tableName );

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableData(
        std::string& tableName );
    */

protected:
    ///
    virtual void QueryTables();

private:
    Poco::SharedPtr< Poco::Data::Statement > m_statement;

};

#endif //MY_SQL_CONNECTION_H
