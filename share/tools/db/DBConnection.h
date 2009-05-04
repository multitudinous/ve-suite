
#ifndef DB_CONNECTION_H
#define DB_CONNECTION_H

// --- VE-Suite Includes --- //
#include "TypeDefs.h"

// --- POCO Includes --- //
#include <Poco/SharedPtr.h>

namespace Poco
{
namespace Data
{
class Session;
//class RecordSet;
}
}

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32

// --- C/C++ Includes --- //
#include <map>

/*!\file DBConnection.h
 *
 */

/*!\class DBConnection
 * 
 */
class DBConnection
{
public:
    ///Constructor
    DBConnection( std::string& name );

    ///Destructor
    virtual ~DBConnection();

    ///
    enum
    {
        MSACCESS = 1,
        MYSQL = 2
    };

    ///
    const bool Connected() const;

    ///
    const unsigned int GetDBType() const;

    ///
    const std::string& GetName() const;

    ///
    const StringVector1D& GetTableNames() const;

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableFieldNames(
        std::string& tableName ) = 0;
    */

    ///
    virtual const StringVector2D* const GetTableDetails(
        std::string& tableName ) = 0;

    /*
    ///
    virtual const Poco::Data::RecordSet* const GetTableData(
        std::string& tableName ) = 0;
    */

protected:
    ///
    virtual void QueryTables() = 0;

    ///
    bool m_connected;

    ///
    unsigned int m_dbType;

    ///
    std::string m_name;

    ///
    StringVector1D m_tableNames;

    ///
    //std::map< std::string, Poco::Data::RecordSet > m_tableFieldNames;

    ///
    std::map< std::string, StringVector2D > m_tableDetails;

    ///
    //std::map< std::string, Poco::Data::RecordSet > m_tableData;

    ///
    Poco::SharedPtr< Poco::Data::Session > m_session;

private:



};

#endif //DB_CONNECTION_H
