#pragma once

#include <Persistence/DataAbstractionLayer.h>
#include <Persistence/Persistable.h>
#include <Persistence/Cache.h>
#include <Persistence/BufferBase.h>
#include <Persistence/Store.h>
#include <Persistence/Multiplexer.h>
#include <Persistence/SearchCriterion.h>

namespace Persistence
{

    class DataManager: public DataAbstractionLayer
{
public:
    DataManager();

    virtual ~DataManager();

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
    void Buffer( std::vector< std::string > ids,
                 BufferBase::BufferPriority priority = BufferBase::NORMAL_PRIORITY );

private:
    DataAbstractionLayerPtr m_buffer;
    DataAbstractionLayerPtr m_dataMultiplexer;
};

} // namespace Persistence
