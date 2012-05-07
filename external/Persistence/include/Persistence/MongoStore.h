#pragma once

#include <Persistence/Store.h>
#include "mongo/client/dbclient.h"

namespace Persistence
{

class MongoStore : public Store
{
public:
    MongoStore();
    ~MongoStore();

    void SetStorePath( const std::string& path );

    /**
     * Checks whether table with @c typeName exists in the current database.
     * @param typeName Name of table to check for
     * @return @c true if table exists, @c false otherwise.
     */
    bool HasTypename( const std::string& typeName );

    virtual void Attach();

    /**
      * Attempts clean shutdown of database connections and deletes the
      * working db file.
      */
    virtual void Detach();

    virtual void Remove( Persistable& persistable );

    virtual bool HasIDForTypename( const boost::uuids::uuid& id, const std::string& typeName );

    virtual void GetIDsForTypename( const std::string& typeName,
                                    std::vector< std::string >& resultIDs );

    virtual void Search( const std::string& typeName,
                         std::vector< SearchCriterion >& criteria,
                         const std::string& returnField,
                         std::vector< std::string >& results );

    virtual void ProcessBackgroundTasks();

    //virtual void SetChild( DataAbstractionLayerPtr child );

    virtual void Drop( const std::string& typeName, Role role = DEFAULT_ROLE  );

    void MapReduce( const std::string& typeName,
                    const std::string& jsMapFunction,
                    const std::string& jsReduceFunction,
                    mongo::BSONObj queryObj,
                    const std::string& outputUUID,
                    const std::string& outputcollection = ""  );

protected:
    virtual void SaveImpl( const Persistable& persistable,
                           Role role = DEFAULT_ROLE  );

    virtual void LoadImpl( Persistable& persistable,
                           Role role = DEFAULT_ROLE );

private:

    unsigned int GetBoostAnyVectorSize( const boost::any& value );

    /// Holds the current db path
    std::string m_path;

    mongo::DBClientConnection* m_connection;
};

} // namespace Persistence
