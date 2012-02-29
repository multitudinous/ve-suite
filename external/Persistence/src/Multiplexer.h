#pragma once

#include "DataAbstractionLayer.h"
#include "Store.h"

namespace Persistence
{

class Multiplexer: public DataAbstractionLayer
{
public:
    Multiplexer();


    // Overrides of DataAbstractionLayer
    virtual void Save( const Persistable& persistable,
                       Role role = DEFAULT_ROLE  );
    virtual void Load( Persistable& persistable, Role role = DEFAULT_ROLE );
    virtual void Remove( Persistable& persistable, Role role );
    virtual void GetIDsForTypename( const std::string& typeName,
                                    std::vector< std::string >& resultIDs,
                                    Role role );
    virtual void Search( const std::string& typeName,
                         /*criteria,*/
                         std::vector< std::string >& resultIDs );

    virtual void Drop( const std::string& typeName, Role role = DEFAULT_ROLE  );

    void ProcessBackgroundTasks();

    // Attaches and detaches stores from the data access flow
    void AttachStore( DataAbstractionLayerPtr store,
                      Store::StoreRole role,
                      Store::SyncMode mode = Store::ASYNCHRONOUS_MODE );
    void DetachStore( DataAbstractionLayerPtr store );

private:
    typedef std::vector< std::pair< DataAbstractionLayerPtr, Store::StoreRole > > StoreListType;
    StoreListType m_stores;

    DataAbstractionLayerPtr m_workingStore;
    std::vector< DataAbstractionLayerPtr > m_backingStores;
    DataAbstractionLayerPtr m_fullVersioningStore;
    //DataAbstractionLayerPtr m_snapshotVersioningStores;
};

} // namespace Persistence
