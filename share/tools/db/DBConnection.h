
#ifndef DB_CONNECTION_H
#define DB_CONNECTION_H

// --- VE-Suite Includes --- //


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
    DBConnection();

    ///Destructor
    virtual ~DBConnection();

    ///
    enum DB
    {
        MYSQL = 7,
        ACCESS = 77
    };

    ///
    const unsigned int GetDBType() const;

protected:
    ///
    unsigned int m_dbType;

private:



};

#endif //DB_CONNECTION_H
