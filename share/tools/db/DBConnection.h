
#ifndef DB_CONNECTION_H
#define DB_CONNECTION_H

// --- VE-Suite Includes --- //

// --- C/C++ Includes --- //
#include <string>
#include <vector>
#include <map>

// --- typedef --- //
typedef std::vector< std::string > StringArray1D;
typedef std::vector< std::vector< std::string > > StringArray2D;

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
        ACCESS = 1,
        MYSQL = 2
    };

    ///
    const unsigned int GetDBType() const;

    ///
    const std::string& GetName() const;

    ///
    const StringArray1D& GetTableNames() const;

    ///
    virtual const StringArray1D* const GetTableFieldNames(
        std::string& tableName ) = 0;

    ///
    virtual const StringArray2D* const GetTableDetails(
        std::string& tableName ) = 0;

    ///
    virtual const StringArray2D* const GetTableData(
        std::string& tableName ) = 0;

protected:
    ///
    virtual void QueryTables() = 0;

    ///
    unsigned int m_dbType;

    ///
    std::string m_name;

    ///
    StringArray1D m_tableNames;

    ///
    std::map< std::string, StringArray1D > m_tableFieldNames;

    ///
    std::map< std::string, StringArray2D > m_tableDetails;

    ///
    std::map< std::string, StringArray2D > m_tableData;

private:



};

#endif //DB_CONNECTION_H
