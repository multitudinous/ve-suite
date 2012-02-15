#pragma once

#include "DataAbstractionLayer.h"
#include "Persistable.h"
#include "Cache.h"
#include "BufferBase.h"
#include "Store.h"
#include "Multiplexer.h"

namespace Persistence
{

    class DataManager: public DataAbstractionLayer
{
public:
    DataManager();
    // Nothing in this class is virtual since it is a singleton that is never
    // meant to be derived from.
    ~DataManager();

    // Overrides of DataAbstractionLayer
    void ProcessBackgroundTasks();
    // I think these two can be subsumed by passing role = VERSION_ROLE into
    // save and load.
    //void VersioningSave( const Persistable& persistable );
    //void VersioningLoad( Persistable& persistable );

    // Management functions common to this class and Multiplexer.
    // The methods in this class are forwarding functions to those in
    // Multiplexer.
    void AttachStore( DataAbstractionLayerPtr store,
                      Store::StoreRole role,
                      Store::SyncMode mode = Store::ASYNCHRONOUS_MODE );
    void DetachStore( DataAbstractionLayerPtr store );

    // Management fucntions unique to this class
    void SetCache( DataAbstractionLayerPtr cache );
    void SetBuffer( DataAbstractionLayerPtr buffer );

    // Shared among this class, Cache, and BufferBase
    void Buffer( std::vector< std::string > ids, BufferBase::BufferPriority priority = BufferBase::NORMAL_PRIORITY );

private:
    DataAbstractionLayerPtr m_buffer;
    DataAbstractionLayerPtr m_dataMultiplexer;
};

} // namespace Persistence
