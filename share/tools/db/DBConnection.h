
#ifndef DB_CONNECTION_H
#define DB_CONNECTION_H

// --- VE-Suite Includes --- //

// --- C/C++ Includes --- //
#include <string>
#include <vector>

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
    const std::vector< std::string >& GetTableNames() const;

protected:
    ///
    virtual void QueryTables() = 0;

    ///
    unsigned int m_dbType;

    ///
    std::string m_name;

    std::vector< std::string > m_tableNames;

private:



};

#endif //DB_CONNECTION_H
