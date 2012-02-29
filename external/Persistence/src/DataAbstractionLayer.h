#pragma once

#include "DataAbstractionLayerPtr.h"
#include "Persistable.h"

namespace Persistence
{

class DataAbstractionLayer
{
public:
    DataAbstractionLayer();
    virtual ~DataAbstractionLayer();

    /// Persistables can be used in two roles:
    enum Role{ WORKING_ROLE = 1,
               BACKING_ROLE = 2,
               DEFAULT_ROLE = 3,
               VERSIONING_ROLE = 4 };

    /// Saves persistable to data store. Unless a version branch is specifically
    /// being tagged, role should be left to the default.
    virtual void Save( const Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Loads persistable from data store.
    virtual void Load( Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Removes persistable from data store.
    virtual void Remove( Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Does this DAL object have a datum with this ID?
    virtual bool HasIDForTypename( const boost::uuids::uuid& id, const std::string& typeName );

    /// Gets all available IDs for a given type.
    virtual void GetIDsForTypename( const std::string& typeName,
                                    std::vector< std::string >& resultIDs );

    /// Gets all available IDs for a given type where criteria are met.
    virtual void Search( const std::string& typeName,
                         /*criteria,*/
                         std::vector< std::string >& resultIDs );


    /// Gives the DAL a chance to do background tasks such as buffering data,
    /// performing heuristic analysis, building indices, etc.
    virtual void ProcessBackgroundTasks();

    /// Sets the store-side child of this DAL. Calls to all other methods in
    /// this interface should be forwarded to the store-side child after any
    /// local processing occurs.
    virtual void SetChild( DataAbstractionLayerPtr child );

    /// Delete all records with typename typeName in all stores of with role Role
    virtual void Drop( const std::string& typeName, Role role = DEFAULT_ROLE  );

protected:
    /// Holds the store-side child of this DAL
    DataAbstractionLayerPtr m_child;
};

} // namespace Persistence
