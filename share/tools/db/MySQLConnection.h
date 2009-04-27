
#ifndef MY_SQL_CONNECTION_H
#define MY_SQL_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- MySQL++ Includes --- //
#include <connection.h>

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

protected:
    ///
    virtual void QueryTables();

private:


};

#endif //MY_SQL_CONNECTION_H
