
#ifndef MY_SQL_CONNECTION_H
#define MY_SQL_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- MySQL++ Includes --- //
#include <connection.h>
#include <query.h>

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32

// --- C/C++ Includes --- //

/*!\file MySQLConnection.h
 *
 */

/*!\class MySQLConnection
 * 
 */
class MySQLConnection : public DBConnection, public mysqlpp::Connection
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

    ///
    virtual const StringArray1D* const GetTableFieldNames(
        std::string& tableName );

    ///
    virtual const StringArray2D* const GetTableDetails(
        std::string& tableName );

    ///
    virtual const StringArray2D* const GetTableData(
        std::string& tableName );

protected:
    ///
    virtual void QueryTables();

private:
    mysqlpp::Query m_query;

};

#endif //MY_SQL_CONNECTION_H
