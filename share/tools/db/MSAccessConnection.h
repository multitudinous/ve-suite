
#ifndef MS_ACCESS_CONNECTION_H
#define MS_ACCESS_CONNECTION_H

// --- VE-Suite Includes --- //
#include "DBConnection.h"

// --- POCO Includes --- //

// --- wxWidgets Includes --- //
/*
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32
*/

// --- C/C++ Includes --- //

/*!\file MSAccessConnection.h
 *
 */

/*!\class MSAccessConnection
 * 
 */
class MSAccessConnection : public DBConnection
{
public:
    ///Constructor
    MSAccessConnection(
        std::string& db,
        std::string& server,
        std::string& username,
        std::string& password,
        unsigned int port );

    ///Destructor
    virtual ~MSAccessConnection();

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

};

#endif //MS_ACCESS_CONNECTION_H
